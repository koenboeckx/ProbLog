%%% -*- Mode: Prolog; -*-

%% Ch. 11 - Mixture Models
%% Unknown Number of Categories

:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).

:- set_options(default),set_inference(backward(lw)).
%:- set_debug(true).

:- use_module(library(gensym)).
builtin(gensym(A,B)).

:- initialization(init).

map_multiply(_, [], []) := true.
map_multiply(N, [H1|T1], [H2|T2]) := 
    H2 is N*H1,
    map_multiply(N, T1, T2).

bags_aux(0, []) := true.    
bags_aux(N, [Bag|Bags]) :=
    gensym('bag', Bag),
    N1 is N-1,
    bags_aux(N1, Bags). 
    

phi ~ dirichlet([1.0,1.0,1.0]).
alpha(10.0) := true.
prototype ~ val(Proto) :=
    alpha(Alpha),
    phi ~= Phi,
    map_multiply(Alpha, Phi, Proto).

bag_prototype(Bag) ~ dirichlet(Proto) :=
    prototype ~= Proto.

temp ~ poisson(1.0) .
num_bags ~ val(N) :=
    temp ~= N1, N is N1+1.

bags ~ val(Bags) :=
    num_bags ~= NumBags,
    bags_aux(NumBags, Bags).

pick_bag(N) ~ uniform(Bags) :=
    bags ~= Bags.

observe_marble(N) ~ finite([Pblue:blue, Pgreen:green, Pred:red]) :=
    pick_bag(N) ~= Bag,
    bag_prototype(Bag) ~= [Pblue, Pgreen, Pred].
    
make_evidence([], []).
make_evidence([Marble|Rest1], [observe_marble(N) ~= Marble|Rest2]) :-
    length(Rest1, N),
    make_evidence(Rest1, Rest2).

test(N) :-
    init,
    make_evidence([red, red, blue, blue, red, blue], Evidence), writeln(Evidence),
    eval_query_distribution_eval(NumBags, Evidence, [],
                                 num_bags ~= NumBags, N, LP, _, _),
    writeln(LP).
 
:- initialization(test(100)).
