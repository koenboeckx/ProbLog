:-use_module(library(lists)).

widget(Widget, ID) :-
    select_weighted(widget(ID), [.05,.1,.2,.3,.2,.1,.05],
                    [.2,.3,.4,.5,.6,.7,.8], Widget, _).

threshold(Threshold) :-
    select_weighted(threshold, [.1,.2,.4,.2,.1],
                    [.3,.4,.5,.6,.7], Threshold, _).

next_good_widget(Widget, ID) :-
    threshold(Threshold),
    subquery(widget(Widget, ID), P, [good(Threshold, ID)]),
    pr(Widget, Threshold, ID, P). % IMPORTANT!

P::pr(_,_,_,P).

good(Threshold, ID) :-
    widget(Widget, ID),
    Widget > Threshold.

query(threshold(_)).

evidence(next_good_widget(.6, 1)). 
evidence(next_good_widget(.7, 2)).     
evidence(next_good_widget(.8, 3)).   
