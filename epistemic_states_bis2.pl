% 6 - Inference about Inference
% Epistemic States - bis
% One button, multiple pushes
% 04/05/17

:-use_module(library(lists)).

0.7::next(go_on, N); 0.3::next(stop, N).

action([], N) :- next(stop, N).
action([], 0).
action([a|R], N) :-
    next(go_on, N), N1 is N-1,
    action(R, N1).
    
action(Action) :-
    between(1, 3, N),
    length(Action, N),
    action(Action, N).

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

0.5::goal(bagel); 0.5::goal(cookie).

effects(Buttons, P) :-
    values(V, 10),
    select_uniform(effects(Buttons), V, P, _).

Pb::vending_machine(Buttons, bagel); Pc::vending_machine(Buttons, cookie) :-
    effects(Buttons, P), Pb is P, Pc is 1.0-P.

transition(Action, Result) :- vending_machine(Action, Result).

evidence(goal(cookie)).
%evidence(vending_machine([a,a], cookie)).
evidence(choose_action([a,a])).
evidence(choose_action([a]), false).

query(effects([a,a], _)).
query(effects([a], _)).
%query(goal(_)).

%query(action(_)).
