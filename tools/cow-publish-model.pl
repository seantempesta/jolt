% cow-publish-model.pl — explicit-state model of the copy-on-write publication
% the concurrent collections share (host/chez/java/jutil-concurrent.ss):
% CopyOnWriteArrayList / CopyOnWriteArraySet publish a fresh vector,
% ConcurrentSkipListMap / ConcurrentSkipListSet a fresh persistent tree
% (tree-map.ss tmr-write / tmr-sm!).
%
%   swipl -q -g main -t halt tools/cow-publish-model.pl   (or `make cowpublishmodel`)
%
% Readers take the published value with no lock and then read into it. Writers
% never change a value once it is published: a writer builds a new one from the
% published one and publishes it with one store, behind a release fence, so a
% reader that loads the new pointer sees the value whole. Two kinds of writer:
%
%   locked      (add, set, put, remove, compute, merge, pollFirst, …)
%     start    take the mutex
%     locked   read the published value (its keys)
%     snap     begin building the new value              (mid-construction)
%     building finish building it: the writer's key is in
%     built    release fence, then publish it: one store
%     pub      release the mutex
%   optimistic  (CopyOnWriteArrayList removeIf / removeAll / retainAll: the
%               test runs on a snapshot outside the lock)
%     ostart   read the published value, no lock
%     osnap    compute the new contents (no shared effect), then take the mutex
%     olocked  if the published value is still the one it read, build and
%              publish as above; else release the mutex and start over
%   reader
%     idle     load the published pointer
%     loaded   read into the value it loaded
%
% Checked, over every interleaving of two locked writers, one optimistic writer
% and two readers: no reader reads into a value under construction (a torn
% read), no deadlock, and once every writer is done the published value holds
% every writer's update (no lost update). Four mutants must be caught:
%   no_fence      the store becomes visible before the build is finished
%                 (a publish without the release fence)          -> torn read
%   in_place      a writer changes the published value itself      -> torn read
%   snap_outside  a locked writer reads the published value before
%                 taking the mutex                                 -> lost update
%   no_recheck    the optimistic writer publishes without checking
%                 the value it read is still the published one     -> lost update

:- dynamic mutant/1.

% writer kinds, by position; two readers
kinds([lock, lock, opt]).
nreaders(2).
writers(Ws) :- kinds(K), length(K, N), numlist(1, N, Ws).
readers(Rs) :- nreaders(N), numlist(1, N, Rs).
kind(W, K) :- kinds(Ks), nth1(W, Ks, K).

% s(Pub, Lock, Building, WriterPcs, ReaderPcs, Torn)
%   Pub = v(Id, Keys): the published value; Building: ids of values not yet whole
init(s(v(0, []), none, [], Ws, Rs, false)) :-
    writers(W), findall(Pc, (member(I, W), kind(I, K), start_pc(K, Pc)), Ws),
    nreaders(NR), length(Rs, NR), maplist(=(idle), Rs).
start_pc(lock, start).
start_pc(opt, ostart).

setnth([_|Xs], 1, Y, [Y|Xs]) :- !.
setnth([X|Xs], N, Y, [X|Ys]) :- N1 is N-1, setnth(Xs, N1, Y, Ys).
add_key(K, Ks, Ks2) :- ( memberchk(K, Ks) -> Ks2 = Ks ; msort([K|Ks], Ks2) ).
del(X, L, L2) :- ( selectchk(X, L, L1) -> L2 = L1 ; L2 = L ).

% ---- a locked writer ------------------------------------------------------------
wstep(s(P,none,B,Ws,Rs,Tn), W, s(P,W,B,Ws2,Rs,Tn)) :-
    nth1(W, Ws, start), \+ mutant(snap_outside), setnth(Ws, W, locked, Ws2).
wstep(s(v(I,K),W,B,Ws,Rs,Tn), W, s(v(I,K),W,B,Ws2,Rs,Tn)) :-
    nth1(W, Ws, locked), \+ mutant(in_place), setnth(Ws, W, snap(K), Ws2).
% mutant: read the published value before taking the mutex
wstep(s(v(I,K),L,B,Ws,Rs,Tn), W, s(v(I,K),L,B,Ws2,Rs,Tn)) :-
    nth1(W, Ws, start), mutant(snap_outside), setnth(Ws, W, early(K), Ws2).
wstep(s(P,none,B,Ws,Rs,Tn), W, s(P,W,B,Ws2,Rs,Tn)) :-
    nth1(W, Ws, early(K)), setnth(Ws, W, snap(K), Ws2).
% building the new value, id W
wstep(s(P,W,B,Ws,Rs,Tn), W, s(P,W,[W|B],Ws2,Rs,Tn)) :-
    nth1(W, Ws, snap(K)), setnth(Ws, W, building(K), Ws2).
wstep(s(P,W,B,Ws,Rs,Tn), W, s(P,W,B2,Ws2,Rs,Tn)) :-
    nth1(W, Ws, building(K)), del(W, B, B2), add_key(k(W), K, K2), setnth(Ws, W, built(K2), Ws2).
wstep(s(_,W,B,Ws,Rs,Tn), W, s(v(W,K),W,B,Ws2,Rs,Tn)) :-
    nth1(W, Ws, built(K)), setnth(Ws, W, pub, Ws2).
% mutant: the store is seen before the build is finished (no release fence)
wstep(s(_,W,B,Ws,Rs,Tn), W, s(v(W,K2),W,B,Ws2,Rs,Tn)) :-
    nth1(W, Ws, building(K)), mutant(no_fence), add_key(k(W), K, K2), setnth(Ws, W, early_pub, Ws2).
wstep(s(P,W,B,Ws,Rs,Tn), W, s(P,W,B2,Ws2,Rs,Tn)) :-
    nth1(W, Ws, early_pub), del(W, B, B2), setnth(Ws, W, pub, Ws2).
% mutant: change the published value in place
wstep(s(v(I,K),W,B,Ws,Rs,Tn), W, s(v(I,K),W,[I|B],Ws2,Rs,Tn)) :-
    nth1(W, Ws, locked), mutant(in_place), setnth(Ws, W, inplace, Ws2).
wstep(s(v(I,K),W,B,Ws,Rs,Tn), W, s(v(I,K2),W,B2,Ws2,Rs,Tn)) :-
    nth1(W, Ws, inplace), del(I, B, B2), add_key(k(W), K, K2), setnth(Ws, W, pub, Ws2).
wstep(s(P,W,B,Ws,Rs,Tn), W, s(P,none,B,Ws2,Rs,Tn)) :-
    nth1(W, Ws, pub), setnth(Ws, W, done, Ws2).

% ---- the optimistic writer ------------------------------------------------------
wstep(s(v(I,K),L,B,Ws,Rs,Tn), W, s(v(I,K),L,B,Ws2,Rs,Tn)) :-
    nth1(W, Ws, ostart), setnth(Ws, W, osnap(I,K), Ws2).
wstep(s(P,none,B,Ws,Rs,Tn), W, s(P,W,B,Ws2,Rs,Tn)) :-
    nth1(W, Ws, osnap(I,K)), setnth(Ws, W, olocked(I,K), Ws2).
% still the value it read: build from it and publish
wstep(s(v(I,K),W,B,Ws,Rs,Tn), W, s(v(I,K),W,B,Ws2,Rs,Tn)) :-
    nth1(W, Ws, olocked(I,K)), setnth(Ws, W, snap(K), Ws2).
% changed meanwhile: let go and start over
wstep(s(v(J,K2),W,B,Ws,Rs,Tn), W, s(v(J,K2),none,B,Ws2,Rs,Tn)) :-
    nth1(W, Ws, olocked(I,_)), J \== I, \+ mutant(no_recheck), setnth(Ws, W, ostart, Ws2).
% mutant: publish what it computed from the stale value anyway
wstep(s(v(J,K2),W,B,Ws,Rs,Tn), W, s(v(J,K2),W,B,Ws2,Rs,Tn)) :-
    nth1(W, Ws, olocked(I,K)), J \== I, mutant(no_recheck), setnth(Ws, W, snap(K), Ws2).

% ---- a reader -------------------------------------------------------------------
rstep(s(v(I,K),L,B,Ws,Rs,Tn), R, s(v(I,K),L,B,Ws,Rs2,Tn)) :-
    nth1(R, Rs, idle), setnth(Rs, R, loaded(I), Rs2).
rstep(s(P,L,B,Ws,Rs,Tn), R, s(P,L,B,Ws,Rs2,Tn2)) :-
    nth1(R, Rs, loaded(I)), setnth(Rs, R, idle, Rs2),
    ( memberchk(I, B) -> Tn2 = true ; Tn2 = Tn ).

next(S, S2) :- writers(Ws), member(W, Ws), wstep(S, W, S2).
next(S, S2) :- readers(Rs), member(R, Rs), rstep(S, R, S2).

reach(R) :- init(I), bfs([I], [I], R).
bfs([], Seen, Seen).
bfs([S|Q], Seen, Out) :-
    findall(S2, next(S, S2), N0), sort(N0, N1),
    exclude([X]>>memberchk(X, Seen), N1, N),
    append(Seen, N, Seen2), append(Q, N, Q2), bfs(Q2, Seen2, Out).

all_done(s(_,_,_,Ws,_,_)) :- maplist(==(done), Ws).
torn(R) :- member(s(_,_,_,_,_,true), R).
% readers can always step, so a deadlock is a state no WRITER can leave unfinished
deadlock(R) :- member(S, R), \+ all_done(S), \+ (writers(Ws), member(W, Ws), wstep(S, W, _)).
lost(R) :- member(S, R), all_done(S), S = s(v(_,K),_,_,_,_,_),
    writers(Ws), member(W, Ws), \+ memberchk(k(W), K).

check(Name, Goal, R) :-
    ( call(Goal, R) -> format("  FAIL ~w~n", [Name]), nb_setval(failed, true)
    ; format("  ok   ~w~n", [Name]) ).
caught(Mutant, Goal) :-
    retractall(mutant(_)), assertz(mutant(Mutant)), reach(R),
    ( call(Goal, R) -> format("  ok   mutant ~w is caught~n", [Mutant])
    ; format("  FAIL mutant ~w was not caught~n", [Mutant]), nb_setval(failed, true) ),
    retractall(mutant(_)).
with_mutant(M, Goal) :- retractall(mutant(_)), assertz(mutant(M)), reach(R), call(Goal, R), retractall(mutant(_)).

% the verdict as one goal, for a solver that answers a query (chiasmus_verify)
verdict(ok) :-
    retractall(mutant(_)), reach(R), \+ torn(R), \+ deadlock(R), \+ lost(R),
    with_mutant(no_fence, torn), with_mutant(in_place, torn),
    with_mutant(snap_outside, lost), with_mutant(no_recheck, lost).

main :-
    nb_setval(failed, false),
    retractall(mutant(_)), reach(R), length(R, N),
    format("2 locked writers, 1 optimistic writer, 2 readers: ~w reachable states~n", [N]),
    check('no torn read', torn, R),
    check('no deadlock', deadlock, R),
    check('no lost update', lost, R),
    caught(no_fence, torn),
    caught(in_place, torn),
    caught(snap_outside, lost),
    caught(no_recheck, lost),
    nb_getval(failed, F),
    ( F == true -> halt(1) ; true ).
