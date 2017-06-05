%%% -*- Mode: Prolog; -*-

% 6 - Inference about Inference
% Preferences - bis
% 04/05/17

:-use_module(library(lists)).

choose_action(Action, ID) :-
    goal(Goal,ID),
    subquery(action(Action), P, [inner(Goal)]),
    pr(Action, Goal, ID, P).
P::pr(_,_,_,P).

inner(Goal) :-
    action(Action),
    transition(Action, Goal).

values(Values, N) :- % select N values between 0 and 1
    findall(Y, (between(0, N, X), Y is X/N), Values).
preference(Preference) :-
    values(Values, 30),
    select_uniform(preference, Values, Preference, _).

0.5::action(a); 0.5::action(b).
P::flip(N) :- preference(P).
goal(bagel, ID) :- flip(ID).
goal(cookie,ID) :- \+flip(ID).


0.9::vending_machine(a, bagel); 0.1::vending_machine(a, cookie).
0.1::vending_machine(b, bagel); 0.9::vending_machine(b, cookie).

transition(Action, Result) :- vending_machine(Action, Result).

query(goal(_,4)). % Set N > 3, to have 'unbiased' distribution
                  % (= not directly linked to observation)

evidence(choose_action(b, 1)).  % or
%evidence(goal(cookie, 1)).     % alternative evidence
evidence(choose_action(b, 2)).
evidence(choose_action(b, 3)).
