%%% -*- Mode: Prolog; -*-

%% Ch 10 - Occam's Razor
% Bayes Occam's Razor

hypo_param(hypo_a, [0.375,0.375,0.125,0.125]).
hypo_param(hypo_b, [0.25,0.25,0.25,0.25]).

0.5::hypothesis(hypo_a); 0.5::hypothesis(hypo_b).

observe(0,[]).
observe(N, [Letter|Others]) :-
    pick_letter(N, Letter),
    N1 is N-1,
    observe(N1, Others).

Pa::pick_letter(N, a); Pb::pick_letter(N, b); Pc::pick_letter(N, c); Pd::pick_letter(N, d) :-
	between(1,100,N),
	hypothesis(Hypo),
    hypo_param(Hypo, [Pa,Pb,Pc,Pd]).

evidence(observe(8, [a,b,a,b,c,d,b,b])).
query(hypothesis(_)).
