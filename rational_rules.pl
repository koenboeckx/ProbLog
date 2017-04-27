% Rational Rules (from Ch. 8. Learning as conditional inference)

% Represent a rules as a list of conjunctions: Rule = [Conj1, Conj2, ...]
% Represent a conjuction as a list of value - attribute pairs:
%   [Att1 = 0/1/?, Att2 = 0/1/?, ...] (-> ? = don't care)

:-use_module(library(lists)).
:-use_module('generate_unique_id.py').  % Help with noisy_equal: generate unique

% Params
tau(0.3).           % stopping probability of grammar
noise_param(X) :-   % noise probability
    X is exp(-5).     % => 1.5 gives X ~= 0.223
max_len_rule(2).    % Maximum # of Conj in a rule
numAttr(2).         % Number of attributes (4 in Rational Rules)

P::flip(P, _, _).
P::flip(P,_).
P::flip(P).

formula_flip(N) :-
    tau(Tau),
    flip(Tau, N, 0).
conj_flip(M, N) :-
    tau(Tau),
    flip(Tau, M, N).

formula([Conj|Formula], N) :- % N is max number of conjunctions
    N > 0,
    formula_flip(N),  % only recursion if myflip is true
    make_conj(Conj, N),
    N1 is N-1,
    formula(Formula, N1).

% 2 stop conditions:
%   1. myflip is false
formula([Conj], N) :-
    N > 0,
    \+formula_flip(N),
    make_conj(Conj, N).
%   2. N = 0
formula([], 0).

%% Make Predicates
make_conj(Conj, RuleID) :-
    numAttr(NumAttr),
    conj(Conj, NumAttr, RuleID).

conj([],0,_).
conj([Symbol|Conj], N, RuleID) :-
    N > 0,
    ID is N + 81*RuleID, % attempt at creating a unique ID
    select_uniform(ID, [0,1,?], Symbol, _), % uncomment to enable  don't care
    %select_uniform(ID, [0,1], Symbol, _),  % uncomment to disable don't care
    N1 is N-1,
    conj(Conj, N1, RuleID).

% eval_pos(Formula, PosExample) => evaluate a formula on a positive example
eval_pos([],_) :- false. % !! empty conj is always false
eval_pos([F1|Rest], Example) :- % a formula is true if first Conj is true:
    eval_conj(F1, Example)
    ;                           % or if a next conjuction in the formula is true:
    eval_pos(Rest, Example).

eval_conj([], []).
eval_conj([C1|FRest], [E1|ERest]) :-
    (C1 = ?;            % a conjuction is true if every predicate is true
    gen_unique_id(ID),
    noisy_equal(C1, E1, ID)),
    eval_conj(FRest, ERest).

P::noisy_equal(A, B, ID) :-
    writenl(ID),
    (A = B, P is 0.9999999; 
    not(A = B), noise_param(P)). 

observe :-
	max_len_rule(Max),
    formula(F, Max),
	eval_pos(F, [1,1]). 

%query(eval_pos([[0,0], [0,0]], [1,1])).

query(formula(F, Max)) :-
    max_len_rule(Max).
evidence(observe).
%query(formula_flip(N)) :-
%	between(1,4,N).
