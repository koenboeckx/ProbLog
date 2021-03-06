%%% -*- Mode: Prolog; -*-

%% Chapter 12. Non-Parametric Models
% Nested Chinese Restaurant Process (OK)


:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).
:- use_module(library(apply)).

:- use_module(library(gensym)).
builtin(gensym(A,B)).

:- set_options(default),set_inference(backward(lw)).
%:- set_debug(true).

:- initialization(init).

% --------------------------------------------------------------------------------------------
% conc(Level, Alpha) := true. % concentration parameter
conc(top, 1.0) := true.
conc(subordinate(_), 1.0) := true.

sticks(Level, Index, Alpha) ~ beta(1, Alpha).
sticks(Level, Index) ~ val(P) :=
    conc(Level, Alpha),
    sticks(Level, Index, Alpha) ~= P.

flip(Level, Index, UniqueID) ~ finite([P1:true, P2:false]) :=
    sticks(Level, Index) ~= P, 
    P1 is P, P2 is 1.0-P.

pick_a_stick(Level, Index, Sample, N) :=
    flip(Level, Index, N) ~= true,
    Sample = Index.

pick_a_stick(Level, Index, Sample, N) :=
    flip(Level, Index, N) ~= false,
    Index1 is Index+1,
    pick_a_stick(Level, Index1, Sample, N).

% --------------------------------------------------------------------------------------------
    
base_distr(top, N) ~ val(X) := gensym('t', X).
base_distr(subordinate, N) ~ val(X) := gensym('s', X).

dPMem(top, N) ~ val(X) :=
    pick_a_stick(top, 1, Index, N),
    base_distr(top, Index) ~= X.

dPMem(subordinate, N) ~ val(c(X, ParentCat)) :=
    dPMem(top, N) ~= ParentCat, 
    pick_a_stick(subordinate(ParentCat), 1, Index, N),
    base_distr(subordinate, Index) ~= X.
    
% --------------------------------------------------------------------------------------------


get_samples(0, []) := true.
get_samples(N, [Sample|Samples]) :=
    N > 0, 
    dPMem(subordinate, N) ~= Sample, 
    N1 is N-1,
    get_samples(N1, Samples).
    
test(N) :-
    init, 
    generate_backward(get_samples(N, Samples), _), 
    writeln(Samples).

:-initialization(test(10)).

