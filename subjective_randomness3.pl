% 23/04/17
% Subjective Randomness

0.5::is_fair(Coin).
coin(C, P) :-
    debugprint('coin', C),
    (is_fair(C),  P is 0.5;
    \+is_fair(C), P is 0.2).
PT::flip(N, C, t); PF::flip(N, C, f) :-
    coin(C, P),
    PT is P, PF is 1-P.
observe(0, _,[]).
observe(N, Coin, [Sample|Samples]) :-
    N > 0,
    flip(N, Coin, Sample),
    N1 is N-1,
    observe(N1, Coin, Samples).

evidence(observe(5, c1, [f,f,t,f,t])). % is '00101' fair?
evidence(observe(5, c2, [f,f,f,f,f])). % is '00000' fair?
query(is_fair(c1)).
query(is_fair(c2)).
