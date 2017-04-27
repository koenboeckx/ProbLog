 % Estimating Causal Power $\label{lst:causal_power}$
% 25/04/17

:-use_module(library(lists)).

values([0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]).
cp(X) :-
    values(Values),
    select_uniform(cp, Values, X, _).
b(X) :-
    values(Values),
    select_uniform(b, Values, X, _).

% e(ID, C).
P::e(_,t) :- % probability of E if C = true
    cp(P).
P::e(_,f) :- % probability of E if C = false
    b(P).
    
evidence(e(1,t), true).
evidence(e(2,t), true).
evidence(e(3,f), false).
evidence(e(4,t), true).
evidence(e(5,t), false).

query(cp(_)).
