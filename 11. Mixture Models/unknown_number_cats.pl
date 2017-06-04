%%% -*- Mode: Prolog; -*-

%% Ch. 11 - Mixture Models
%% Unknown Number of Categories

:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).
:- use_module(library(apply)).

:- set_options(default),set_inference(backward(lw)).
%:- set_debug(true).

:- initialization(init).

actual_obs([true, true, true, true, false, false, false, false]).

coins ~ finite([0.5:[c1], 0.5:[c1,c2]]).

coin_weight(Coin) ~ beta(1,1).

flip(Coin, N) ~ finite([PT:true, PF:false]) :=
    coin_weight(Coin) ~= PT,
    PF is 1.0-PT.
    
pick_coin(N) ~ uniform(Coins) :=
    coins ~= Coins.

observe([]) := true.
observe([V|Values]) :=
    length(Values, N),
    pick_coin(N) ~= Coin,
    flip(Coin, N) ~= V, 
    observe(Values).

test(N) :-
    init, 
    actual_obs(Observations), 
    query([observe(Observations)], [], (coins ~= [_,_]), N, P),
    writeln(P).
 
:- initialization(test(100000)).
