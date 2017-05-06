% 6 - Inference about Inference
% Epistemic States - bis
% 04/05/17

:-use_module(library(lists)).

choose_action(Action) :-
    goal(Goal), transition(Action, Goal),
    subquery(action(Action), P),
    pr(Action, Goal, P).
P::pr(_,_,P).

inner(Goal) :-
    action(Action),
    transition(Action, Goal).

values(Values, N) :- % select N values between 0 and 1
    findall(Y, (between(0, N, X), Y is X/N), Values).

0.5::action(a);   0.5::action(b).
0.5::goal(bagel); 0.5::goal(cookie).

a_effects(P) :-
    values(V, 10),
    select_uniform(a_effects, V, P, _).
b_effects(P) :-
    values(V, 10),
    select_uniform(b_effects, V, P, _).

Pb::vending_machine(a, bagel); Pc::vending_machine(a, cookie) :-
    a_effects(P), Pb is P, Pc is 1.0-P.
Pb::vending_machine(b, bagel); Pc::vending_machine(b, cookie) :-
    b_effects(P), Pb is P, Pc is 1.0-P.

transition(Action, Result) :- vending_machine(Action, Result).

evidence(goal(cookie)).
evidence(choose_action(b)).
evidence(choose_action(a), false).

query(a_effects(_)).
query(b_effects(_)).

