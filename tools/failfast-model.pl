% failfast-model.pl — explicit-state model of the fail-fast iteration the
% java.util list and deque shims implement (host/chez/java/host-static-classes.ss).
%
%   swipl -q -g main -t halt tools/failfast-model.pl   (or `make failfastmodel`)
%
% Part 1, ArrayList / LinkedList: a modCount. A list carries a count every
% structural change bumps (add, insert, remove; not set). An iterator records
% the count it expects and is #(cursor lastRet expected):
%   hasNext  ArrayList: cursor =\= size; LinkedList: cursor < size. Never checks.
%   next     the count first (ConcurrentModificationException), then the bound
%            (NoSuchElementException), then hand out element cursor.
%   remove   lastRet < 0 is IllegalStateException; then the count; then remove
%            element lastRet, step the cursor back to it, and expect the new count.
% Checked over every sequence of iterator steps and foreign changes (add,
% insert at 0, remove, set) up to a bound, on a list of distinct tokens, against
% a ghost: the iterator is CLEAN until a foreign structural change, and
%   clean      next never throws CME; the first `cursor` elements are exactly
%              the ones it handed out and did not remove, in order (no element
%              skipped or handed out twice); hasNext is false only once every
%              element has been handed out
%   dirty      next throws CME, and so does remove once something was handed out
%   always     hasNext never throws
% Mutants that must be caught:
%   no_bump        a foreign remove does not bump the count     -> dirty miss
%   no_resync      remove does not take the new count            -> clean CME
%   no_step_back   remove leaves the cursor where it was         -> skip
%   bound_first    next checks the bound before the count        -> dirty miss
%   hasnext_checks hasNext checks the count                      -> hasNext throws
%
% Part 3, the hash and tree families (HashMap / HashSet / LinkedHashMap /
% LinkedHashSet / TreeMap / TreeSet and their views): a modCount on the map
% and an iterator that walks a SNAPSHOT of the keys taken when it is made,
% #(remaining lastRet expected). Bump rules (the JDK's): a put / add of a new
% key, a remove of a present one and every clear bump the count; a put over an
% existing key, a remove of an absent one do not. next checks the count, then
% the end (HashMap$HashIterator) or the end, then the count
% (TreeMap$PrivateEntryIterator); remove checks lastRet, then the count, then
% removes and takes the new count. Checked over every sequence of iterator
% steps and foreign puts / removes / clears up to a bound against a ghost:
%   clean      no foreign structural change yet: what was handed out and kept,
%              then the snapshot's remainder, is exactly the live collection in
%              its order (the snapshot walk IS the live walk), and next never
%              throws a CME
%   dirty      next throws CME (the tree's: unless the snapshot is used up, where
%              it is NoSuchElementException), remove after a next throws CME
% Mutants that must be caught:
%   put_existing_bumps  a put over an existing key bumps        -> clean CME
%   absent_remove_bumps a remove of an absent key bumps          -> clean CME
%   no_clear_bump       a clear of an empty map does not bump   -> dirty miss
%   ff_no_resync        remove does not take the new count       -> clean CME
%   tree_count_first    the tree checks the count before the end -> wrong error
%
% Part 2, ArrayDeque: a ring buffer, as the JDK's (no modCount). State
% d(Slots, Head, Tail): nil marks an empty slot; it is full when an add makes
% Head = Tail, which grows it at once (capacity c -> c + c + 2 below 64), with a
% wrapped first leg slid to the end. delete(i) shifts whichever side of i is
% shorter. Checked over every op sequence (addFirst, addLast, pollFirst,
% pollLast, delete at each live index, iterator steps with its own removes)
% from a capacity-2 deque up to a bound, against a plain list:
%   refine     the slots read from Head to Tail are the list
%   ring       exactly those slots are non-nil, and the buffer is never full
%   walk       an iterator with no foreign change hands out every element once,
%              in order, its own removes included (the cursor steps back when
%              delete shifted the back half left)
% Mutants:
%   no_grow        an add that fills the buffer does not grow it   -> ring
%   no_slide       grow does not slide a wrapped first leg          -> refine
%   always_back    the iterator steps back after every own remove   -> walk
%   no_postdelete  the iterator ignores which half delete shifted   -> walk

:- dynamic mutant/1.

% ---------------------------------------------------------------- shared
setnth([_|Xs], 0, Y, [Y|Xs]) :- !.
setnth([X|Xs], N, Y, [X|Ys]) :- N1 is N - 1, setnth(Xs, N1, Y, Ys).
delnth([_|Xs], 0, Xs) :- !.
delnth([X|Xs], N, [X|Ys]) :- N1 is N - 1, delnth(Xs, N1, Ys).
take(0, _, []) :- !.
take(N, [X|Xs], [X|Ys]) :- N1 is N - 1, take(N1, Xs, Ys).

% ================================================================ part 1
% l(Elems, MC); i(Cursor, LastRet, Exp); g(Clean, Kept): Kept are the tokens
% handed out and not removed by the iterator, in order. Fresh tokens come from
% a counter so every element is distinct.
kind(al). kind(ll).

has_next(al, C, Es) :- length(Es, N), C =\= N.
has_next(ll, C, Es) :- length(Es, N), C < N.

% the implementation's steps: step(Kind, Op, S0, S, Result)
% S = st(l(Es,MC), i(C,LR,Exp), g(Clean,Kept), Fresh)
bump(MC, MC1) :- MC1 is MC + 1.

step(_, next, st(l(Es,MC), i(C,LR,Exp), G, F), S, R) :-
    (   mutant(bound_first), length(Es, N), C >= N
    ->  R = nsee, S = st(l(Es,MC), i(C,LR,Exp), G, F)
    ;   Exp =\= MC
    ->  R = cme, S = st(l(Es,MC), i(C,LR,Exp), G, F)
    ;   length(Es, N), C >= N
    ->  R = nsee, S = st(l(Es,MC), i(C,LR,Exp), G, F)
    ;   nth0(C, Es, X), C1 is C + 1, G = g(Cl, K), append(K, [X], K1),
        R = val(X), S = st(l(Es,MC), i(C1,C,Exp), g(Cl,K1), F)
    ).
step(Kind, has_next, st(L, i(C,LR,Exp), G, F), st(L, i(C,LR,Exp), G, F), R) :-
    L = l(Es, MC),
    (   mutant(hasnext_checks), Exp =\= MC -> R = cme
    ;   has_next(Kind, C, Es) -> R = true ; R = false ).
step(_, iremove, st(l(Es,MC), i(C,LR,Exp), G, F), S, R) :-
    (   LR < 0
    ->  R = ise, S = st(l(Es,MC), i(C,LR,Exp), G, F)
    ;   Exp =\= MC
    ->  R = cme, S = st(l(Es,MC), i(C,LR,Exp), G, F)
    ;   nth0(LR, Es, X), delnth(Es, LR, Es1), bump(MC, MC1),
        ( mutant(no_step_back) -> C1 = C ; C1 = LR ),
        ( mutant(no_resync) -> Exp1 = Exp ; Exp1 = MC1 ),
        G = g(Cl, K), ( Cl == true -> exclude(==(X), K, K1) ; K1 = K ),
        R = ok, S = st(l(Es1,MC1), i(C1,-1,Exp1), g(Cl,K1), F)
    ).
step(_, fadd, st(l(Es,MC), I, g(_,K), F), st(l(Es1,MC1), I, g(false,K), F1), ok) :-
    append(Es, [t(F)], Es1), bump(MC, MC1), F1 is F + 1.
step(_, fins0, st(l(Es,MC), I, g(_,K), F), st(l([t(F)|Es],MC1), I, g(false,K), F1), ok) :-
    bump(MC, MC1), F1 is F + 1.
step(_, frem(J), st(l(Es,MC), I, g(_,K), F), st(l(Es1,MC1), I, g(false,K), F), ok) :-
    length(Es, N), J < N, delnth(Es, J, Es1),
    ( mutant(no_bump) -> MC1 = MC ; bump(MC, MC1) ).
step(_, fset(J), st(l(Es,MC), i(C,LR,Exp), g(Cl,K), F), st(l(Es1,MC), i(C,LR,Exp), g(Cl,K1), F1), ok) :-
    length(Es, N), J < N, nth0(J, Es, Old), setnth(Es, J, t(F), Es1), F1 is F + 1,
    % set is no structural change: a token the iterator kept is now the new one
    ( Cl == true, J < C -> replace_tok(K, Old, t(F), K1) ; K1 = K ).
replace_tok([], _, _, []).
replace_tok([X|Xs], O, N, [Y|Ys]) :- ( X == O -> Y = N ; Y = X ), replace_tok(Xs, O, N, Ys).

ops1(N, Ops) :-
    numlist(0, 2, Is),
    findall(frem(I), member(I, Is), Rs), findall(fset(I), member(I, Is), Ss),
    append([[next, has_next, iremove, fadd, fins0], Rs, Ss], Ops0), Ops = Ops0, N = N.

% the property each step must keep; violation(Kind, Op, S0, S, R, Why)
violation(_, Op, st(_, _, g(true,_), _), _, cme, clean_cme) :- Op == next.
violation(_, has_next, _, _, cme, hasnext_throws).
violation(_, next, st(_, _, g(false,_), _), _, R, dirty_miss) :- R \== cme.
violation(_, iremove, st(_, i(_,LR,_), g(false,_), _), _, R, dirty_miss) :- LR >= 0, R \== cme.
violation(_, _, _, st(l(Es,_), i(C,_,_), g(true,K), _), _, prefix) :-
    length(Es, N), C =< N, take(C, Es, P), P \== K.
violation(_, _, _, st(l(Es,_), i(C,_,_), g(true,_), _), _, cursor_past_end) :-
    length(Es, N), C > N.

% depth-bounded search from every initial list of 0..3 elements
init1(N, st(l(Es,0), i(0,-1,0), g(true,[]), F)) :-
    numlist(1, N, Ns), findall(t(X), member(X, Ns), Es), F is N + 1.
init1(0, st(l([],0), i(0,-1,0), g(true,[]), 1)).

explore1(_, _, 0, _) :- !, fail.
explore1(Kind, S0, D, Why) :-
    ops1(_, Ops), member(Op, Ops),
    step(Kind, Op, S0, S, R),
    (   violation(Kind, Op, S0, S, R, Why0) -> Why = Why0-Op
    ;   D1 is D - 1, explore1(Kind, S, D1, Why) ).

% hasNext false while clean means the list holds exactly what was kept
end_check(Kind, Why) :-
    member(N, [0,1,2,3]), init1(N, S0), reach_end(Kind, S0, 5, Why).
reach_end(_, _, 0, _) :- !, fail.
reach_end(Kind, S0, D, Why) :-
    ops1(_, Ops), member(Op, Ops), step(Kind, Op, S0, S, R),
    (   Op == has_next, R == false, S = st(l(Es,_), _, g(true,K), _), Es \== K -> Why = hasnext_early
    ;   D1 is D - 1, reach_end(Kind, S, D1, Why) ).

bad1(Kind, Why) :- kind(Kind), member(N, [0,1,2,3]), init1(N, S0), explore1(Kind, S0, 5, Why).
bad1(Kind, Why) :- kind(Kind), end_check(Kind, Why).

% ================================================================ part 3
% s3(Live, MC, Remaining, LastRet, Exp, Clean, Kept); keys are 0..2, Live keeps
% insertion order for a hash family (hash) and key order for a tree (tree).
fam(hash). fam(tree).
ins(hash, K, L, L1) :- append(L, [K], L1).
ins(tree, K, L, L1) :- msort([K|L], L1).

ff_next(Fam, Rem, Exp, MC, R) :-
    (   Fam == tree, \+ mutant(tree_count_first)
    ->  ( Rem == [] -> R = nsee ; Exp =\= MC -> R = cme ; R = go )
    ;   ( Exp =\= MC -> R = cme ; Rem == [] -> R = nsee ; R = go ) ).

step3(Fam, next, s3(L,MC,Rem,LR,Exp,Cl,K), S, R) :-
    ff_next(Fam, Rem, Exp, MC, R0),
    (   R0 == go
    ->  Rem = [X|Rem1], append(K, [X], K1), R = val(X),
        S = s3(L,MC,Rem1,X,Exp,Cl,K1)
    ;   R = R0, S = s3(L,MC,Rem,LR,Exp,Cl,K) ).
step3(_, iremove, s3(L,MC,Rem,LR,Exp,Cl,K), S, R) :-
    (   LR == none -> R = ise, S = s3(L,MC,Rem,LR,Exp,Cl,K)
    ;   Exp =\= MC -> R = cme, S = s3(L,MC,Rem,LR,Exp,Cl,K)
    ;   exclude(==(LR), L, L1), MC1 is MC + 1,
        ( mutant(ff_no_resync) -> Exp1 = Exp ; Exp1 = MC1 ),
        exclude(==(LR), K, K1), R = ok, S = s3(L1,MC1,Rem,none,Exp1,Cl,K1) ).
step3(Fam, fput(Key), s3(L,MC,Rem,LR,Exp,Cl,K), s3(L1,MC1,Rem,LR,Exp,Cl1,K), ok) :-
    between(0, 2, Key),
    (   memberchk(Key, L)
    ->  L1 = L, Cl1 = Cl, ( mutant(put_existing_bumps) -> MC1 is MC + 1 ; MC1 = MC )
    ;   ins(Fam, Key, L, L1), MC1 is MC + 1, Cl1 = false ).
step3(_, frem(Key), s3(L,MC,Rem,LR,Exp,Cl,K), s3(L1,MC1,Rem,LR,Exp,Cl1,K), ok) :-
    between(0, 2, Key),
    (   memberchk(Key, L)
    ->  exclude(==(Key), L, L1), MC1 is MC + 1, Cl1 = false
    ;   L1 = L, Cl1 = Cl, ( mutant(absent_remove_bumps) -> MC1 is MC + 1 ; MC1 = MC ) ).
step3(_, fclear, s3(L,MC,Rem,LR,Exp,_,K), s3([],MC1,Rem,LR,Exp,false,K), ok) :-
    ( mutant(no_clear_bump), L == [] -> MC1 = MC ; MC1 is MC + 1 ).

viol3(_, next, s3(_,_,_,_,_,true,_), cme, clean_cme).
viol3(_, iremove, s3(_,_,_,_,_,true,_), cme, clean_cme).
viol3(hash, next, s3(_,_,_,_,_,false,_), R, dirty_miss) :- R \== cme.
viol3(tree, next, s3(_,_,Rem,_,_,false,_), R, dirty_miss) :- ( Rem == [] -> R \== nsee ; R \== cme ).
viol3(_, iremove, s3(_,_,_,LR,_,false,_), R, dirty_miss) :- LR \== none, R \== cme.
viol3_after(s3(L,_,Rem,_,_,true,K), refine) :- append(K, Rem, W), W \== L.

init3(Fam, s3(L,0,L,none,0,true,[])) :-
    member(Ks, [[], [0], [0,1], [1,0,2]]), ( Fam == tree -> msort(Ks, L) ; L = Ks ).
ops3([next, iremove, fput(0), fput(1), fput(2), frem(0), frem(1), frem(2), fclear]).
explore3(_, _, 0, _) :- !, fail.
explore3(Fam, S0, D, Why) :-
    ops3(Ops), member(Op, Ops), step3(Fam, Op, S0, S, R),
    (   viol3(Fam, Op, S0, R, W) -> Why = W-Op
    ;   viol3_after(S, W) -> Why = W-Op
    ;   D1 is D - 1, explore3(Fam, S, D1, Why) ).
bad3(Fam, Why) :- fam(Fam), init3(Fam, S0), explore3(Fam, S0, 5, Why).

% ================================================================ part 2
% d(Slots, Head, Tail) and the reference list; values are distinct tokens.
dq_cap(d(S,_,_), C) :- length(S, C).
inc(I, C, J) :- J0 is I + 1, ( J0 >= C -> J = 0 ; J = J0 ).
dec(I, C, J) :- J0 is I - 1, ( J0 < 0 -> J is C - 1 ; J = J0 ).
sub(I, J, C, K) :- K0 is I - J, ( K0 < 0 -> K is K0 + C ; K = K0 ).

dq_grow(d(S,H,T), D) :-
    length(S, C), ( C < 64 -> Jump is C + 2 ; Jump is C >> 1 ), NC is C + Jump,
    Space is NC - C, length(Pad, Space), maplist(=(nil), Pad), append(S, Pad, S1),
    nth0(H, S1, HV),
    (   \+ mutant(no_slide), ( T < H ; T =:= H, HV \== nil )
    ->  % slide [H, C) up by Space, nil the gap
        length(Pre, H), append(Pre, Rest, S1), Leg is C - H, take(Leg, Rest, LegVals),
        length(Gap, Space), maplist(=(nil), Gap),
        append([Pre, Gap, LegVals], S2), length(S2, NC),
        H1 is H + Space, D = d(S2, H1, T)
    ;   D = d(S1, H, T) ).

dq_add_last(d(S,H,T), X, D) :-
    length(S, C), setnth(S, T, X, S1), inc(T, C, T1),
    ( H =:= T1, \+ mutant(no_grow) -> dq_grow(d(S1,H,T1), D) ; D = d(S1,H,T1) ).
dq_add_first(d(S,H,T), X, D) :-
    length(S, C), dec(H, C, H1), setnth(S, H1, X, S1),
    ( H1 =:= T, \+ mutant(no_grow) -> dq_grow(d(S1,H1,T), D) ; D = d(S1,H1,T) ).
dq_poll_first(d(S,H,T), X, D) :-
    nth0(H, S, X), X \== nil, length(S, C), setnth(S, H, nil, S1), inc(H, C, H1), D = d(S1,H1,T).
dq_poll_last(d(S,H,T), X, D) :-
    length(S, C), dec(T, C, T1), nth0(T1, S, X), X \== nil, setnth(S, T1, nil, S1), D = d(S1,H,T1).

% delete the element at physical index I: Shift = left when the back half moved
dq_delete(d(S,H,T), I, D, Shift) :-
    length(S, C), sub(I, H, C, Front), sub(T, I, C, B0), Back is B0 - 1,
    (   Front < Back
    ->  shift_front(S, H, I, C, S1), setnth(S1, H, nil, S2), inc(H, C, H1),
        D = d(S2, H1, T), Shift = right
    ;   dec(T, C, T1), shift_back(S, I, T1, C, S1), setnth(S1, T1, nil, S2),
        D = d(S2, H, T1), Shift = left ).
% move each element from I-1 down to H one slot forward (cyclically)
shift_front(S, H, I, C, S) :- I =:= H, !, C = C.
shift_front(S, H, I, C, R) :- dec(I, C, P), nth0(P, S, V), setnth(S, I, V, S1), shift_front(S1, H, P, C, R).
% move each element from I+1 up to T1 one slot back
shift_back(S, I, T1, C, S) :- I =:= T1, !, C = C.
shift_back(S, I, T1, C, R) :- inc(I, C, N), nth0(N, S, V), setnth(S, I, V, S1), shift_back(S1, N, T1, C, R).

dq_list(d(S,H,T), L) :- length(S, C), dq_walk(S, H, T, C, L).
dq_walk(_, I, T, _, []) :- I =:= T, !.
dq_walk(S, I, T, C, [X|Xs]) :- nth0(I, S, X), inc(I, C, J), dq_walk(S, J, T, C, Xs).

ring_ok(d(S,H,T)) :-
    length(S, C), H < C, T < C,
    dq_list(d(S,H,T), L), length(L, N), N < C,
    \+ member(nil, L),
    findall(x, (member(V, S), V \== nil), NN), length(NN, N).

% physical index of the K-th live element
phys(d(S,H,_), K, I) :- length(S, C), I is (H + K) mod C.

% ops on (D, Ref, Fresh)
dq_op(add_last, D0-R0-F, D-R-F1) :- dq_add_last(D0, t(F), D), append(R0, [t(F)], R), F1 is F + 1.
dq_op(add_first, D0-R0-F, D-[t(F)|R0]-F1) :- dq_add_first(D0, t(F), D), F1 is F + 1.
dq_op(poll_first, D0-[_|R]-F, D-R-F) :- dq_poll_first(D0, _, D).
dq_op(poll_last, D0-R0-F, D-R-F) :- R0 \== [], dq_poll_last(D0, _, D), append(R, [_], R0).
dq_op(delete(K), D0-R0-F, D-R-F) :- length(R0, N), N1 is N - 1, between(0, N1, K),
    phys(D0, K, I), dq_delete(D0, I, D, _), delnth(R0, K, R).

dq_bad(Why) :-
    dq_init(S0), dq_explore(S0, 7, Why).
dq_init(d([nil,nil],0,0)-[]-1).
dq_explore(_, 0, _) :- !, fail.
dq_explore(S0, Dp, Why) :-
    member(Op, [add_last, add_first, poll_first, poll_last, delete(_)]),
    dq_op(Op, S0, S), S = D-R-_,
    (   \+ ring_ok(D) -> Why = ring-Op
    ;   dq_list(D, L), L \== R -> Why = refine-Op
    ;   Dp1 is Dp - 1, dq_explore(S, Dp1, Why) ).

% the iterator: it(Cursor, Remaining, LastRet) over the deque, its own removes
% via delete; with no foreign change it must hand out every element once, in
% order, whatever it removes along the way
dq_walk_bad(Why) :-
    dq_init(S0), dq_build(S0, 5, D-R-_), dq_iterate(D, R, Why).
dq_build(S, _, S).
dq_build(S0, N, S) :- N > 0, member(Op, [add_last, add_first, poll_first, delete(_)]),
    dq_op(Op, S0, S1), N1 is N - 1, dq_build(S1, N1, S).
dq_iterate(d(S,H,T), Ref, Why) :-
    length(Ref, N), length(Choices, N), dq_choices(Choices),
    dq_run(d(S,H,T), it(H, N, -1), Choices, [], Out, Why0),
    (   nonvar(Why0) -> Why = Why0
    ;   Out \== Ref -> Why = walk ).
dq_choices([]).
dq_choices([C|Cs]) :- member(C, [keep, remove]), dq_choices(Cs).
dq_run(_, it(_,0,_), [], Acc, Out, _) :- !, reverse(Acc, Out).
dq_run(D, it(Cur,Rem,_), [Ch|Chs], Acc, Out, Why) :-
    D = d(S,_,_), length(S, C), nth0(Cur, S, X),
    (   X == nil -> Why = walk_nil, Out = []
    ;   inc(Cur, C, Cur1), Rem1 is Rem - 1,
        (   Ch == remove
        ->  dq_delete(D, Cur, D1, Shift),
            (   mutant(always_back) -> dec(Cur1, C, Cur2)
            ;   Shift == left, \+ mutant(no_postdelete) -> dec(Cur1, C, Cur2)
            ;   Cur2 = Cur1 ),
            dq_run(D1, it(Cur2,Rem1,-1), Chs, [X|Acc], Out, Why)
        ;   dq_run(D, it(Cur1,Rem1,Cur), Chs, [X|Acc], Out, Why) ) ).

% ================================================================ driver
check(Label, Goal) :-
    (   Goal -> format("  FAIL ~w~n", [Label]), Res = fail
    ;   format("  ok   ~w~n", [Label]), Res = ok ),
    nb_setval(last, Res).

main :-
    format("fail-fast model~n"),
    retractall(mutant(_)),
    ( bad1(K1, W1) -> format("  FAIL modCount: ~w ~w~n", [K1, W1]), halt(1) ; format("  ok   modCount protocol (ArrayList and LinkedList hasNext)~n") ),
    ( bad3(F3, W3a) -> format("  FAIL hash/tree modCount: ~w ~w~n", [F3, W3a]), halt(1) ; format("  ok   modCount protocol (hash and tree snapshot iterators)~n") ),
    ( dq_bad(W2) -> format("  FAIL ring buffer: ~w~n", [W2]), halt(1) ; format("  ok   ring buffer refines a list, never full~n") ),
    ( dq_walk_bad(W3) -> format("  FAIL deque iterator: ~w~n", [W3]), halt(1) ; format("  ok   deque iterator walk with its own removes~n") ),
    forall(member(M-G, [no_bump-bad1(_,_), no_resync-bad1(_,_), no_step_back-bad1(_,_),
                        bound_first-bad1(_,_), hasnext_checks-bad1(_,_),
                        put_existing_bumps-bad3(_,_), absent_remove_bumps-bad3(_,_), no_clear_bump-bad3(_,_),
                        ff_no_resync-bad3(_,_), tree_count_first-bad3(_,_),
                        no_grow-dq_bad(_), no_slide-dq_bad(_), always_back-dq_walk_bad(_),
                        no_postdelete-dq_walk_bad(_)]),
           ( retractall(mutant(_)), assertz(mutant(M)),
             ( call(G) -> format("  ok   mutant ~w caught~n", [M])
             ; format("  FAIL mutant ~w NOT caught~n", [M]), halt(1) ),
             retractall(mutant(_)) )),
    format("fail-fast model: passed~n").
