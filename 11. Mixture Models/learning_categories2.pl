%%% -*- Mode: Prolog; -*-

%% Ch. 11 - Mixture Models
%% Learning categories - program 2: Mixture Distribution

:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).
:- use_module(library(apply)).

:- set_options(default),set_inference(backward(lw)).
%:- set_debug(true).

:- initialization(init).

% Auxiliary
map_multiply(_, [], []) := true. %% !!
map_multiply(N, [H1|T1], [H2|T2]) := %% !!
    H2 is N*H1,
    map_multiply(N, T1, T2).

phi ~ dirichlet([1,1,1]).
alpha ~ val(0.1).
prototype ~ val(Ps) :=
	phi ~= Phi,
	alpha ~= Alpha,
	map_multiply(Alpha, Phi, Ps).
bag_prototype(Bag) ~ dirichlet(Prototype) :=
	prototype ~= Prototype.

bag_mixture ~ dirichlet([1,1,1]).

obs_bag(Obs) ~ finite([P1:bag1,P2:bag2,P3:bag3]) :=
	bag_mixture ~= [P1,P2,P3].

draw_marble(Obs) ~ finite([Pb:blue, Pg:green, Pr:red]) :=
	obs_bag(Obs) ~= Bag,
	bag_prototype(Bag) ~= [Pb,Pg,Pr].

evidence([	draw_marble(obs1) ~= red,  draw_marble(obs2) ~= red, draw_marble(obs3) ~= blue,
		 	draw_marble(obs4) ~= blue, draw_marble(obs5) ~= red, draw_marble(obs6) ~= blue]).   

test(N) :-
	init,
	evidence(Evidence),
	query(Evidence,[], (obs_bag(obs1) ~= Bag1, obs_bag(obs2) ~= Bag2, Bag1 = Bag2), N, P1),
	write('obs1 and obs2 from same bag: '), writeln(P1),
	query(Evidence,[], (obs_bag(obs1) ~= Bag1, obs_bag(obs3) ~= Bag2, Bag1 = Bag2), N, P2),
	write('obs1 and obs3 from same bag: '), writeln(P2).

:- initialization(test(1000)).
