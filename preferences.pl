%%% -*- Mode: Prolog; -*-

P::choose_action(Goal, Transition, Action, N) :-
    call(Goal, GoalState, N), % identifier N because multiple pushes
    subquery(action_prior(Action), P, [call(Transition, GoalState)]).

0.5::action_prior(a); 0.5::action_prior(b). 
                                       
% food-preference
values([0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]).
:-use_module(library(lists)).
food_pref(P) :-
    values(Values),
    select_uniform(food_pref, Values, P, _).

P::flip(N) :- food_pref(P).
goal_food(Goal, N) :-
    (flip(N),  Goal=bagel;
    \+flip(N), Goal=cookie).
goal(Outcome, N) :-
    goal_food(Outcome, N).

0.9::vending_machine(bagel); 0.1::vending_machine(cookie) :-
    action_prior(a).
0.1::vending_machine(bagel); 0.9::vending_machine(cookie) :-
    action_prior(b).

query(goal_food(_,4)).  % Set N > 3, to have 'unbiased' distribution
                        % (= not directly linked to observation)
% query(food_pref(_)).

evidence(choose_action(goal, vending_machine, b, 1)). % Sally pushes b, or ...
%evidence(goal(cookie, 1)). % ...If Sally says she wants cookie
evidence(choose_action(goal, vending_machine, b, 2)).
evidence(choose_action(goal, vending_machine, b, 3)).
