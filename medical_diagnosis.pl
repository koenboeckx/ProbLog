% Medical Diagnosis Example

% Model
0.2::smokes.

0.001::lung_disease.
0.1::lung_disease :- smokes.
0.02::cold.

0.01::cough.
0.5::cough :- cold.
0.5::cough :- lung_disease.

0.01::fever.
0.3::fever :- cold.

0.01::chest_pain.
0.2::chest_pain :- lung_disease.

0.01::shortness_of_breath.
0.2::shortness_of_breath :- lung-disease.

% Evidence
evidence(cough, true).
evidence(chest_pain, false).


% Queries
query(cold).
query(lung_disease).
