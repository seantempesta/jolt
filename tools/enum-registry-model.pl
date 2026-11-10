% enum-registry-model.pl — explicit-state model of the enum model's tables
% (host/chez/java/enums.ss enum-register!: enum-class-tbl and enum-const-tbl).
%
%   swipl -q -g main -t halt tools/enum-registry-model.pl   (or `make enumregistrymodel`)
%
% Writers declare enums (register-enum-class!, jolt.host/register-enum!) while
% readers look constants up (enum-of, enum-class-info) with no lock. A Chez
% hashtable is not safe to read while another thread resizes it, so a writer
% never touches the table readers see: under enum-mu it copies the published
% table, fills the copy, and publishes the copy with one store. Each step below
% is one atomic transition:
%
%   writer   start    take enum-mu
%            locked   copy the published table (a fresh table, id = the writer)
%            copied   begin filling the copy           (the copy is mid-mutation)
%            filling  finish filling: its key is in     (no longer mid-mutation)
%            filled   publish the copy: one store of the table pointer
%            pub      release enum-mu
%   reader   idle     load the published table pointer
%            loaded   look the key up in the table it loaded
%
% Checked, over every interleaving of two writers and two readers: no reader
% ever looks up in a table that is mid-mutation (a torn read), no deadlock, and
% once every writer is done the published table holds every writer's key (no
% lost registration). Two mutants must be caught: filling the PUBLISHED table in
% place (torn read), and copying before taking the mutex (lost registration).

:- dynamic mutant/1.

nwriters(2).
nreaders(2).
writers(Ws) :- nwriters(N), numlist(1, N, Ws).
readers(Rs) :- nreaders(N), numlist(1, N, Rs).

% s(Pub, Lock, Mut, WriterPcs, ReaderPcs, Torn)  Pub = t(Id, Keys)
init(s(t(0, []), none, [], Ws, Rs, false)) :-
    nwriters(NW), length(Ws, NW), maplist(=(start), Ws),
    nreaders(NR), length(Rs, NR), maplist(=(idle), Rs).

setnth([_|Xs], 1, Y, [Y|Xs]) :- !.
setnth([X|Xs], N, Y, [X|Ys]) :- N1 is N-1, setnth(Xs, N1, Y, Ys).
add_key(K, Ks, Ks2) :- ( memberchk(K, Ks) -> Ks2 = Ks ; msort([K|Ks], Ks2) ).
del(X, L, L2) :- ( selectchk(X, L, L1) -> L2 = L1 ; L2 = L ).

wstep(s(P,none,M,Ws,Rs,Tn), W, s(P,W,M,Ws2,Rs,Tn)) :-
    nth1(W, Ws, start), \+ mutant(copy_outside), setnth(Ws, W, locked, Ws2).
% mutant: snapshot the table before taking the mutex
wstep(s(t(I,K),L,M,Ws,Rs,Tn), W, s(t(I,K),L,M,Ws2,Rs,Tn)) :-
    nth1(W, Ws, start), mutant(copy_outside), setnth(Ws, W, snap(K), Ws2).
wstep(s(P,none,M,Ws,Rs,Tn), W, s(P,W,M,Ws2,Rs,Tn)) :-
    nth1(W, Ws, snap(K)), setnth(Ws, W, copied(K), Ws2).
wstep(s(t(I,K),W,M,Ws,Rs,Tn), W, s(t(I,K),W,M,Ws2,Rs,Tn)) :-
    nth1(W, Ws, locked), \+ mutant(in_place), setnth(Ws, W, copied(K), Ws2).
wstep(s(P,W,M,Ws,Rs,Tn), W, s(P,W,[W|M],Ws2,Rs,Tn)) :-
    nth1(W, Ws, copied(K)), setnth(Ws, W, filling(K), Ws2).
wstep(s(P,W,M,Ws,Rs,Tn), W, s(P,W,M2,Ws2,Rs,Tn)) :-
    nth1(W, Ws, filling(K)), del(W, M, M2), add_key(k(W), K, K2), setnth(Ws, W, filled(K2), Ws2).
wstep(s(_,W,M,Ws,Rs,Tn), W, s(t(W,K),W,M,Ws2,Rs,Tn)) :-
    nth1(W, Ws, filled(K)), setnth(Ws, W, pub, Ws2).
wstep(s(P,W,M,Ws,Rs,Tn), W, s(P,none,M,Ws2,Rs,Tn)) :-
    nth1(W, Ws, pub), setnth(Ws, W, done, Ws2).
% mutant: fill the published table in place
wstep(s(t(I,K),W,M,Ws,Rs,Tn), W, s(t(I,K),W,[I|M],Ws2,Rs,Tn)) :-
    nth1(W, Ws, locked), mutant(in_place), setnth(Ws, W, inplace, Ws2).
wstep(s(t(I,K),W,M,Ws,Rs,Tn), W, s(t(I,K2),W,M2,Ws2,Rs,Tn)) :-
    nth1(W, Ws, inplace), del(I, M, M2), add_key(k(W), K, K2), setnth(Ws, W, pub, Ws2).

rstep(s(t(I,K),L,M,Ws,Rs,Tn), R, s(t(I,K),L,M,Ws,Rs2,Tn)) :-
    nth1(R, Rs, idle), setnth(Rs, R, loaded(I), Rs2).
rstep(s(P,L,M,Ws,Rs,Tn), R, s(P,L,M,Ws,Rs2,Tn2)) :-
    nth1(R, Rs, loaded(I)), setnth(Rs, R, idle, Rs2),
    ( memberchk(I, M) -> Tn2 = true ; Tn2 = Tn ).

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
lost(R) :- member(S, R), all_done(S), S = s(t(_,K),_,_,_,_,_),
    writers(Ws), member(W, Ws), \+ memberchk(k(W), K).

check(Name, Goal, R) :-
    ( call(Goal, R) -> format("  FAIL ~w~n", [Name]), nb_setval(failed, true)
    ; format("  ok   ~w~n", [Name]) ).
caught(Mutant, Goal) :-
    retractall(mutant(_)), assertz(mutant(Mutant)), reach(R),
    ( call(Goal, R) -> format("  ok   mutant ~w is caught~n", [Mutant])
    ; format("  FAIL mutant ~w was not caught~n", [Mutant]), nb_setval(failed, true) ),
    retractall(mutant(_)).

% the verdict as one goal, for a solver that answers a query (chiasmus_verify)
verdict(ok) :- reach(R), \+ torn(R), \+ deadlock(R), \+ lost(R),
    retractall(mutant(_)), assertz(mutant(in_place)), reach(R1), torn(R1),
    retractall(mutant(_)), assertz(mutant(copy_outside)), reach(R2), lost(R2),
    retractall(mutant(_)).

main :-
    nb_setval(failed, false),
    retractall(mutant(_)), reach(R), length(R, N),
    format("~w writers, ~w readers: ~w reachable states~n", [2, 2, N]),
    check('no torn read', torn, R),
    check('no deadlock', deadlock, R),
    check('no lost registration', lost, R),
    caught(in_place, torn),
    caught(copy_outside, lost),
    nb_getval(failed, F),
    ( F == true -> halt(1) ; true ).
