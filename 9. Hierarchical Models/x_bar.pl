%% Ch. 9 - Hierarchical Models
%% Example: X-Bar Theory

:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(gensym)).

:- set_options(default),set_inference(backward(lw)).
%:- set_debug(true).

:- initialization(init).
builtin(gensym(_,_)). % for generation of unique id's

%% Choose a data set:
data([[d, n]]).
%data([[d, n], [d,n]]).
%data([[d, n], [t,v], [v, adv]]).

categories([d,n,t,v,a,adv]) := true.

% head_comp(Head, Comp)
head_comp(d,n) := true.
head_comp(t,v) := true.
head_comp(n,a) := true.
head_comp(v,adv) := true.
head_comp(a,none) := true.
head_comp(adv,none) := true.

language_direction ~ beta(1, 1).

flip(P, ID) ~ finite([PL:left, PR:right]) :=
    PL is P, PR is 1.0-P.

temp(Head, ID) ~ dirichlet([LangDir, LangDir1]) := % for use in head_phrase
    language_direction ~= LangDir,
    LangDir1 is 1.0-LangDir.

head_phrase(Head, ID) ~ val(P) :=
    temp(Head, ID) ~= [P, _].

generate_phrase(Head) ~ val([Head]) := % no complement
    head_comp(Head, none).

generate_phrase(Head) ~ val([Head, Comp]) := % Comp right of Head
    \+head_comp(Head, none), 
    gensym(g, Unique), 
    head_phrase(Head, Unique) ~= Dir,
    flip(Dir, Unique) ~= Direction,
    flip(Dir, Unique) ~=  right, 
    head_comp(Head, Comp).

generate_phrase(Head) ~ val([Comp, Head]) := % Comp left of Head
    \+head_comp(Head, none),
    gensym(g, Unique),
    head_phrase(Head, Unique) ~= Dir,
    flip(Dir, Unique) ~= Direction,
    head_comp(Head, Comp).

uniform_draw(ID) ~ uniform(Categories) :=  
    categories(Categories).
observe_phrase(ID) ~ val(Phrase) :=
    uniform_draw(ID) ~= Category,
    generate_phrase(Category) ~= Phrase.

% Generate list of obervations based on data term
generate_evidence([], []).
generate_evidence([Obs|Data], [observe_phrase(N) ~= Obs|Evidence]) :-
    gensym(obs, N),
    generate_evidence(Data, Evidence).
    

test(N) :-
    init,
    data(Data),
    generate_evidence(Data, Evidence), writeln(Evidence),
    eval_query_distribution_eval(X, Evidence, [], 
                                 generate_phrase(n) ~= X, N, LP, _, _),
    writeln(LP).

:- initialization(test(200000)).
