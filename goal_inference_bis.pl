% 6 - Inference about Inference
% Goal Inference - bis
% 04/05/17

choose_action(Action) :-
    goal(Goal),
    subquery(action(Action), P, [inner(Goal)]),
    pr(Action, Goal, P).
P::pr(_,_,P).

inner(Goal) :-
    action(Action),
    transition(Action, Goal).

0.5::action(a); 0.5::action(b).
0.5::goal(bagel); 0.5::goal(cookie).

0.9::vending_machine(a, bagel); 0.1::vending_machine(a, cookie).
0.5::vending_machine(b, bagel); 0.5::vending_machine(b, cookie).

transition(Action, Result) :- vending_machine(Action, Result).

query(goal(_)).

evidence(choose_action(b)).

