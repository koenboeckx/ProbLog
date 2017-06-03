%%% -*- Mode: Prolog; -*-

%% Chapter 12. Non-Parametric Models
% Dirichlet Process


:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).
:- use_module(library(apply)).

:- use_module(library(gensym)).
builtin(gensym(A,B)).

:- set_options(default),set_inference(backward(lw)).
%:- set_debug(true).

:- initialization(init).

alpha(1.0) := true. % concentration parameter

sticks(Index, Alpha) ~ beta(1, Alpha).
sticks(Index) ~ val(P) :=
    alpha(Alpha),
    sticks(Index, Alpha) ~= P.

flip(Index, UniqueID) ~ finite([P1:true, P2:false]) :=
    sticks(Index) ~= P, 
    P1 is P, P2 is 1.0-P.

pick_a_stick(Index, Sample, N) :=
    flip(Index, N) ~= true, 
    Sample = Index.

pick_a_stick(Index, Sample, N) :=
    flip(Index, N) ~= false, 
    Index1 is Index+1,
    pick_a_stick(Index1, Sample, N).

base_distr(N) ~ gaussian(0.0, 1.0).
%base_distr(N) ~ val(X) := gensym('', X).
dPMem(N) ~ val(X) :=
    pick_a_stick(1, Index, N),
    base_distr(Index) ~= X.

get_samples(0, []) := true.
get_samples(N, [Sample|Samples]) :=
    N > 0,
    %pick_a_stick(1, Sample, N),
    dPMem(N) ~= Sample, 
    N1 is N-1,
    get_samples(N1, Samples).
    
test(N) :-
    init,
    generate_backward(get_samples(N, Samples), _), 
    writeln(Samples).

:-initialization(test(10000)).
