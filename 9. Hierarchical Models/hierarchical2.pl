%% Use a GLOBAL prototype for all the bags

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

prototype_temp ~ dirichlet([1,1,1,1,1]).
global_prototype ~ val(Proto) :=
    prototype_temp ~= Ps,
    map_multiply(5, Ps, Proto).
% use global prototype
bag_prototype(Bag) ~ dirichlet(Prototype) :=
    global_prototype ~= Prototype.

marble(Bag, _) ~ finite([Pblack:black, Pblue:blue, Pgreen:green,
                         Porange:orange, Pred:red]) :=
    bag_prototype(Bag) ~= Prototype,
    Prototype = [Pblack, Pblue, Pgreen, Porange, Pred].

draw_marbles(Bag, N, Evidence, LP) :-
    length(Evidence, Length),
    Next is Length+1,
    eval_query_distribution_eval(X, Evidence, [], marble(Bag, Next) ~= X, N, LP, _, _).
    
evidence(   [ % Evidence of bag 1
	     marble(bag1, 1) ~= blue, marble(bag1, 2) ~= blue,
	     marble(bag1, 3) ~= black, marble(bag1, 4) ~= blue,
	     marble(bag1, 5) ~= blue, marble(bag1, 6) ~= black,
	      % Evidence of bag 2
             marble(bag2, 1) ~= blue, marble(bag2, 2) ~= green,
              % Evidence of bag 3
             marble(bag3, 1) ~= blue, marble(bag3, 2) ~= blue, 
             marble(bag3, 3) ~= blue, marble(bag3, 4) ~= blue, 
             marble(bag3, 5) ~= blue, marble(bag3, 6) ~= orange]).

test_bags(N) :-
    init,
    evidence(Evidence),
    draw_marbles(bag1, N, Evidence, P1),
    draw_marbles(bag2, N, Evidence, P2),
    draw_marbles(bag3, N, Evidence, P3),
    draw_marbles(bagn, N, Evidence, P4), % no evidence about bag N
    write('Bag 1: '), writeln(P1),
    write('Bag 2: '), writeln(P2),
    write('Bag 3: '), writeln(P3),
    write('Bag N: '), writeln(P4).

 
:- initialization(test_bags(10000)). 
