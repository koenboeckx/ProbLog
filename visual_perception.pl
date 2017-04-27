%%% -*- Mode: Prolog; -*-
% 20/04/17

%% Chapter 4. Patterns of Inference
% A Case Study in Modularity
% use with "Plots from eval_query_distribution.ipynb"


:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).
:- use_module(library(apply)).

:- set_options(default),set_inference(backward(lw)).
%:- set_debug(true).

:- initialization(init).

observed_luminance(3.0) := true.

reflectance ~ gaussian(1, 1).
illumination ~ gaussian(3, 0.5).
luminance ~ val(Prod) :=
    reflectance ~= R,
    illumination ~= I,
    Prod is  R*I.
observation ~ gaussian(ObservedLuminance, 0.1) :=
    observed_luminance(ObservedLuminance).
 
almost_equal(A, B) :=
    A1 is A*0.99, A2 is A*1.01,
    B > A1, B < A2.
 
 test(N) :-
    init,
    eval_query_distribution(X2, [(luminance ~= L, observation ~= O,
                                  almost_equal(L,O),
                                  illumination ~= I, almost_equal(I, 2.0))],
                            [], reflectance ~= X2, N, LP, _,_),
    writeln(LP). 

:- initialization(test(1000000)).
