%%% -*- Mode: Prolog; -*-

%% Learning as conditional inferences
%% For performance evaluation

:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).

:- set_options(default),set_inference(backward(lw)).
%:- set_debug(true).

:- initialization(init).

fair_prior(0.999) := true.
is_fair ~ finite([PT:true, PF:false]) :=
    fair_prior(PT),
    PF is 1.0-PT.

coin_flip(N) ~ finite([0.5:h, 0.5:t]) :=
    is_fair ~= true.
coin_flip(N) ~ finite([0.95:h, 0.05:t]) :=
    is_fair ~= false.

% observations(N, Obs)
observations(0, []) := true.
observations(N, [Sample|Rest]) :=
    coin_flip(N) ~= Sample, 
    N1 is N-1,
    observations(N1, Rest).
    
observed_data([h,h,h,h,h, h,h,h,h,h, h,h,h,h,h, h,h]).

test(NSamples) :-
    init,
    observed_data(ObservedData),
    length(ObservedData, L),
    query([observations(L, ObservedData)], [], is_fair ~= true, NSamples, P),
    writeln(P).

:- initialization(test(10000)).
