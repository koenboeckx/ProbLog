:-use_module(library(lists)).

% fair coin, probability of H = 0.5
observed_data([h,h,t,h,t,h,h,h,t,h]).

% ?? suspicious coincidence, probability of H = 0.5 ..?
%observed_data([h,h,h,h,h,h,h,h,h,h]).

% probably unfair coin, probability of H near 1
%observed_data([h,h,h,h,h,h,h,h,h,h,h,h,h,h,h]).

% definitely unfair coin, probability of H near 1
%observed_data([h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h]).

% unfair coin, probability of H = 0.85
%observed_data([h,h,h,h,h,h,t,h,t,h,h,h,h,h,t,h,h,t,h,h,h,h,h,t,h,t,h,h,h,h,h,t,h,h,h,h,h,h,h,h,h,t,h,h,h,h,t,h,h,h,h,h,h,h]).

weights([0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]).

P::make_coin(Coin, P).

fair_prior(0.999).
P::fair_coin :-
    fair_prior(P).

coin_weight(0.5) :-
    fair_coin.
coin_weight(P) :-
    \+ fair_coin,
    weights(W), select_uniform(cw, W, P, _). % Beta(1,1) = Uniform distribution

coin(C) :- 
    coin_weight(P),
    make_coin(C,P).

sample(C,h) :-
    coin(C).
sample(C,t) :-
    \+coin(C).

observe(0, []).
observe(N, [Sample|Rest]) :-
    sample(N, Sample),
    N1 is N-1,
    observe(N1, Rest).

evidence(observe(N,S)) :-
    observed_data(S),
    length(S,N).

query(fair_coin).
query(coin_weight(_)).
