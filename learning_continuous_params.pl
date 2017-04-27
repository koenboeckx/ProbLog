% Learning Continuous Parameters

:-use_module(library(lists)).
weights([0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]).
coin_weight(X) :-
    weights(Weights),
    select_uniform(weight, Weights, X, _).

PH::coin_flip(h,N); PT::coin_flip(t,N) :-
    coin_weight(P),
    PH is P, PT is 1.0-P.

observe([],0).
observe([Face|T], N) :-
    coin_flip(Face,N),
    N1 is N-1,
    observe(T, N1).

query(coin_weight(_)).

observed_sequence([h,h,t,h,h]).
evidence(observe(S, N)) :-
    observed_sequence(S),
    length(S,N).
