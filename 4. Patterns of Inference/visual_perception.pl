:-use_module('gaussian.py').
:-use_module(library(lists)).

%query(gaussian_probs(21, 5.0, 1.0, Vals, Ws)).

n(11).

reflectance(R) :-
	n(N), 
    gaussian_probs(N, 1.0, 0.5, Vals, Ws),
    select_weighted(reflectance, Ws, Vals, R, _).

illumination(I) :-
	n(N), 
    gaussian_probs(N, 3.0, 0.5, Vals, Ws),
    select_weighted(illumination, Ws, Vals, I, _).

luminance(L) :-
	reflectance(R), illumination(I),
	L is R*I.

observed_luminance(L) :-
	n(N), 
    gaussian_probs(N, 3.0, 0.1, Vals, Ws),
    select_weighted(observed_luminance, Ws, Vals, L, _).

observation1 :-
	luminance(L),
	observed_luminance(OL),
	L = OL.

lower_illumination(I) :- % in shadow of cylinder
	n(N), 
    gaussian_probs(N, 2.0, 0.5, Vals, Ws),
    select_weighted(lower_illumination, Ws, Vals, I, _).

observation2 :-
	illumination(I),
	lower_illumination(LI),
	I = LI.
	
	
evidence(observation1).
evidence(observation2).
query(reflectance(_)).
