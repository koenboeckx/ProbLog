%%% -*- Mode: Prolog; -*-

%% Ch. 11 - Mixture Models
%% Latent Dirichlet Allocation

:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).
:- use_module(library(apply)).

:- set_options(default),set_inference(backward(lw)).
%:- set_debug(true).

:- initialization(init).


%% Auxilaries
% make_list(N, Item, List) => List = [Item, Item, ...]
make_list(0,_,[]) := true.
make_list(N, Item, [Item|Rest]) :=
    N > 0,
    N1 is N-1,
    make_list(N1, Item, Rest).
    
zip([],[],[]) := true.
zip([H1|T1], [H2|T2], [H1:H2|T3]) := 
    zip(T1,T2,T3).

zip([],[],[]).
zip([H1|T1], [H2|T2], [H1:H2|T3]) :-
    zip(T1,T2,T3).

get_all_until(N, L) :=
    findall(X, between(1, N, X), L).

sum_dist([], [0.0, 0.0, 0.0, 0.0]).    
sum_dist([_:[W1,W2,W3,W4]|Rest], [Cum1, Cum2, Cum3, Cum4]) :-
    sum_dist(Rest, [RCum1, RCum2, RCum3, RCum4]),
    Cum1 is RCum1+W1, Cum2 is RCum2+W2, Cum3 is RCum3+W3, Cum4 is RCum4+W4.
mean_dist(List, [Prob1, Prob2, Prob3, Prob4]) :-
   sum_dist(List, [Cum1, Cum2, Cum3, Cum4]),
   length(List, L),
   Prob1 is Cum1/L, Prob2 is Cum2/L, Prob3 is Cum3/L, Prob4 is Cum4/L.

%% Generative Model
topics([topic1, topic2]) := true.
vocabulary([dna, evolution, parsing, phonology]) := true.
doc_length(4) := true. % Must correspond with length of docs in evidence
doc_length(DocID, Length) :=
    doc_length(Length).

doc_mixparams(DocID) ~ dirichlet(L) :=
    topics(Topics),
    length(Topics, N),
    make_list(N, 1.0, L).

topic_mixparams(Topic) ~ dirichlet(L) :=
    vocabulary(Voc),
    length(Voc, N),
    make_list(N, 1.0, L).

topics_mixparams([], []) := true.
topics_mixparams([Topic|Topics], [Param|Params]) :=
    topic_mixparams(Topic) ~= Param,
    topics_mixparams(Topics, Params).

pick_topic(DocID, N) ~ finite(L) :=
    topics(Topics),
    doc_mixparams(DocID) ~= Params,
    zip(Params, Topics, L).

pick_word(Topic, N) ~ finite(L) :=
    vocabulary(Voc),
    topic_mixparams(Topic) ~= Params,
    zip(Params, Voc, L).


doc_topics(DocID) ~ val(Topics) :=
    doc_length(DocID, N),
    doc_topics(DocID, N, Topics).

doc_topics(DocID, 0, []) := true.
doc_topics(DocID, N, [Topic|Topics]) :=
    N > 0,
    pick_topic(DocID, N) ~= Topic,
    N1 is N-1,
    doc_topics(DocID, N1, Topics).
    
doc_words(DocID) ~ val(Words) :=
    doc_length(DocID, N), 
    doc_topics(DocID) ~= Topics,
    doc_words(DocID, N, Topics, Words).

doc_words(_, 0, [], []) := true.
doc_words(DocID, N, [Topic|Topics], [Word|Words]) :=
    N > 0,
    pick_word(Topic, N) ~= Word,
    N1 is N-1,
    doc_words(DocID, N1, Topics, Words).
    
evidence([doc_words(a1) ~= [dna, evolution, dna, evolution],
          %doc_words(a2) ~= [dna, evolution],
          doc_words(b1) ~= [parsing, phonology, parsing, phonology]
          %doc_words(b2) ~= [parsing, phonology]
         ]).
  

test(N) :-
    init,
    generate_backward((doc_words(a1) ~= V1, doc_words(a2) ~= V2),L),
    writeln(V1), writeln(V2),
    evidence(Evidence), writeln(Evidence),
    eval_query_distribution_eval(X, Evidence, [], topic_mixparams(topic1) ~= X, N, LP, _, _),
    %writeln(LP), 
    writeln(' '),
    mean_dist(LP, S), zip([dna, evolution, parsing, phonology], S, VS), writeln(VS).

:- initialization(test(1000000)).

