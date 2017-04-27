%%% -*- Mode: Prolog; -*-

%% Ch 10 - Occam's Razor
% The Rectangle Game

:-use_module(library(lists)).

max(10).
set(Set) :-
    max(Max), findall(X, between(0, Max, X), Set).
set(Min,Max,Set) :-
     findall(X, between(Min, Max, X), Set).
     
x1(X) :- set(Set), select_uniform(x1, Set, X, _).
x2(X) :- set(Set), select_uniform(x2, Set, X, _).
y1(Y) :- set(Set), select_uniform(y1, Set, Y, _).
y2(Y) :- set(Set), select_uniform(y2, Set, Y, _).

concept(X, Y, N) :-
    x1(X1), x2(X2),
    X2 > X1,
    set(X1, X2, SetX), select_uniform([x,N], SetX, X, _),
    y1(Y1), y2(Y2),
    Y2 > Y1,
    set(Y1, Y2, SetY), select_uniform([y,N], SetY, Y, _).

evidence(concept(4, 7, 1)).
evidence(concept(5, 4, 2)).

query(x1(_)).
query(y1(_)).
query(x2(_)).
query(y2(_)).
