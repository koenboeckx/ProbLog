 % Working with sequences

% Prior
0.5::prob(0.2); 0.5::prob(0.7).  % IMPORTANT: no identifier!!!       

% Thunk
PT::myflip(t, N); PF::myflip(f, N) :- % IMPORTANT: with identifier!!!
    prob(P),
    PT is P, PF is 1.0-P.

% repeat(Thunk, Times, L).
repeat(_, 0, []).
repeat(Thunk, N, [Result|T]) :-
    N > 0,
    call(Thunk, Result, N),
    N1 is N-1,
    repeat(Thunk, N1, T).

% Aux functions
first(H) :-
    sequence([H|_]).
second(H) :-
    sequence([_,H|_]).

sequence(S) :-
    repeat(myflip, 3, S).

%evidence(first(f)). % uncomment to see impact
query(second(_)).
