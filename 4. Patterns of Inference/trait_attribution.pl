0.8::exam_fair(E) :- exam(E).
0.8::does_homework(S) :- student(S).

0.9::pass(S, E) :- exam_fair(E), does_homework(S).
0.4::pass(S, E) :- exam_fair(E), \+does_homework(S).
0.6::pass(S, E) :- \+exam_fair(E), does_homework(S).
0.2::pass(S, E) :- \+exam_fair(E), \+does_homework(S).

exam(exam1).
student(bill).
student(mary).
student(tim).

% For query purposes:
joint(t, t) :- exam_fair(exam1), does_homework(bill).
joint(f, t) :- \+exam_fair(exam1), does_homework(bill).
joint(t, f) :- exam_fair(exam1), \+does_homework(bill).
joint(f, f) :- \+exam_fair(exam1), \+does_homework(bill).

query(does_homework(bill)).
query(exam_fair(exam1)).
query(joint(_,_)).

evidence(pass(bill, exam1), false).
evidence(pass(mary, exam1), false).
evidence(pass(tim, exam1), false).
