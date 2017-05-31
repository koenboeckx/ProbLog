%%% -*- Mode: Prolog; -*-

% Learning about Coins - Performance Evaluation

0.999::fair_coin.
trick_coin :- \+fair_coin.

0.5::coin_flip(h,N); 0.5::coin_flip(t,N) :-
    fair_coin.
0.95::coin_flip(h,N); 0.05::coin_flip(t,N) :-
    \+fair_coin.

observe([],0).
observe([Face|T], N) :-
    coin_flip(Face,N),
    N1 is N-1,
    observe(T, N1).

query(fair_coin).
query(trick_coin).

observed_sequence([h,h,h,h,h, h,h,h,h,h, h,h,h,h,h, h,h]).
evidence(observe(S, N)) :-
    observed_sequence(S),
    length(S,N).
