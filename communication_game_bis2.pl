%%% -*- Mode: Prolog; -*-
% 6 - Inference about Inference
% A Communication Game - bis
% 04/05/17

:-use_module(library(lists)).

%% Parameters:
% depth of reasoning:
depth(1).

P::pr(_,P).
    
1/3::side(red,D); 1/3::side(green,D); 1/3::side(blue,D).

0.5::die(a, D); 0.5::die(b, D).

0.0::roll(a, red); 0.2::roll(a, green); 0.8::roll(a, blue).
0.1::roll(b, red); 0.3::roll(b, green); 0.6::roll(b, blue).

%% teacher(+Die, ?Side, +Depth).         
teacher(Die, Side, Depth) :-
    side(Side, Depth),
    subquery(die(Die, Depth), P, [teacher_evidence(Side, Depth)]),
    pr([teacher, Die, Side, Depth], P).

teacher_evidence(Side, Depth) :-
    die(Die, Depth), Depth1 is Depth-1,
    learner(Side, Die, Depth1).

%% learner(+Side, ?Die, +Depth)
learner(Side, Die, Depth) :-
    die(Die, Depth),
    subquery(side(Side, Depth), P, [learner_evidence(Die, Depth)]),
    pr([learner, Side, Die, Depth], P).

learner_evidence(Die, Depth) :-
    Depth > 0,
    side(Side, Depth),
    teacher(Die, Side, Depth).
learner_evidence(Die, 0) :-
    side(Side, 0),
    roll(Die, Side).

received(Side) :- % introduce the given side as evidence
    depth(D),     % this will normalize the result
    learner(Side, _, D).
evidence(received(green)).

query(learner(_, _, D)) :-
    depth(D).
