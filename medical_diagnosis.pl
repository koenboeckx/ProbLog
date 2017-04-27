% independent variables
0.2::smokes.
0.001::lung_disease.
0.02::cold.

% dependent variables
0.1::lung_disease :- smokes.

% symptoms
0.5::cough :- cold.
0.5::cough :- lung_disease.
0.01::cough.

0.3::fever :- cold.
0.01::fever.

0.2::chest_pain :- lung_disease.
0.01::chest_pain.

0.2::shortness_of_breath :- lung_disease.
0.01::shortness_of_breath.

% for query purposes
both(f,f) :- \+ cold, \+lung_disease.
both(t,f) :- cold, \+lung_disease.
both(f,t) :- \+ cold, lung_disease.
both(t,t) :- cold, lung_disease.

query(cold).
query(lung_disease).
query(both(_,_)).

evidence(cough, true).
