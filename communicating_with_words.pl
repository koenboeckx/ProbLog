%%% -*- Mode: Prolog; -*-
% 6 - Inference about Inference
% Communicating with Words - bis
% 04/05/17

:-use_module(library(lists)).

%% Parameters:
% depth of reasoning:
depth(1).

P::pr(_,P).
    
0.25::state(0,D); 0.25::state(1,D); 0.25::state(2,D); 0.25::state(3,D).
words(Words, D) :-
    select_uniform(words(D), [all, some, none], Words, _).

eval_words(all,  3).
eval_words(some, State) :- State > 0.
eval_words(none, 0).

%% speaker(+State, ?Words, +Depth).         
speaker(State, Words, Depth) :-
    words(Words, Depth),
    subquery(state(State, Depth), P, [speaker_evidence(Words, Depth)]),
    pr([speaker, State, Words, Depth], P).

speaker_evidence(Words, Depth) :-
    state(State, Depth), Depth1 is Depth-1,
    listener(Words, State, Depth1).

%% listener(+Words, ?State, +Depth)
listener(Words, State, Depth) :-
    state(State, Depth),
    subquery(words(Words, Depth), P, [listener_evidence(State, Depth)]),
    pr([listener, Words, State, Depth], P).

listener_evidence(State, Depth) :-
    Depth > 0,
    words(Words, Depth),
    speaker(State, Words, Depth).
listener_evidence(State, 0) :-
    words(Words, 0),
    eval_words(Words, State).
    
received(Word) :- % introduce the heard word as evidence
    depth(D),     % this will normalize the result
    listener(Word, _, D).
evidence(received(some)).

query(listener(some, _, D)) :-
    depth(D).
