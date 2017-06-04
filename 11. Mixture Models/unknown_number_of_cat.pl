%%% -*- Mode: Prolog; -*-

%% Ch. 11 - Mixture Models
%% Unknown Number of Categories

:-use_module(library(lists)).

actual_obs([true, true, true, true, false, false, false, false]).

n_weights(5).

0.5::coins([c1]); 0.5::coins([c1, c2]).
weights(Ws) :-
    n_weights(N),
    findall(X, (between(0,N,Y), X is Y/N), Ws).

weight(Coin, W) :-
    weights(Ws), 
    select_uniform(Coin, Ws, W, _).

PT::flip(P, N, true); PF::flip(P, N, false) :-
    PT is P, PF is 1.0-P.

pick_coin(Coins, Coin, N) :-
    select_uniform(pick_coin(N), Coins, Coin, _).

observe([]).
observe([Value|Values]) :-
    length(Values, N), % ID for sample
    coins(Coins), 
    pick_coin(Coins, Coin, N), 
    weight(Coin, W),
    flip(W, N, Value),
    observe(Values).

evidence(observe(Obs)) :-
    actual_obs(Obs).

length_coins(N) :-
    coins(Coins),
    length(Coins, N).
query(length_coins(_)).
