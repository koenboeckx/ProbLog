%%% -*- Mode: Prolog; -*-

%% Chapter 12. Non-Parametric Models
% Infinite Relational Model


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
conc(0.01) := true. % concentration parameter

sticks(Index, Alpha) ~ beta(1, Alpha).
sticks(Index) ~ val(P) :=
    conc(Alpha),
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

%base_distr(N) ~ gaussian(1.0, 1.0).
base_distr(N) ~ val(X) := gensym('class', X).
dPMem(N) ~ val(X) :=
    pick_a_stick(1, Index, N),
    base_distr(Index) ~= X.

% --------------------------------------------------------------------------------------------

object_class(Object) ~ val(Class) :=
    dPMem(Object) ~= Class.

classes_params(Class1, Class2) ~ beta(0.5, 0.5).
talks(Object1, Object2) ~ finite([PT:true, PF:false]) :=
    object_class(Object1) ~= Class1, 
    object_class(Object2) ~= Class2,
    classes_params(Class1, Class2) ~= PT, PF is 1.0-PT.

evidence :=
    talks(tom, fred) ~= true,
    talks(tom, jim) ~= true,

    talsk(jim, fred) ~= false,
    talks(mary, fred) ~= false,
    talks(mary, jim) ~= false,
    talks(sue, fred) ~= false,
    talks(sue, tom) ~= false,
    talks(ann, jim) ~= false,
    talks(ann, tom) ~= false,
    talks(mary, sue) ~= true,
    talks(mary, ann) ~= true,
    talks(ann, sue) ~= true.

    

test(N) :-
    init, 
    query([evidence], [], (object_class(tom) ~= Class1, object_class(fred) ~= Class1), N, P),
    writeln(P).

:-initialization(test(2000)).
