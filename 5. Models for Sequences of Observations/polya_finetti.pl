% Polya Urn with de Finetti method
 :-use_module(library(lists)). 

%beta_weight(+X,+A,+B,?W).
beta_weight(X,A,B,W) :-
    P1 is X**(A-1),
    P2 is (1.0-X)**(B-1),
    W is P1*P2.

%apply_beta(+List,+A,+B,?ListOut).
apply_beta([],_,_,[]).
apply_beta([In|InRest],A,B,[Out|OutRest]) :-
    beta_weight(In,A,B,Out),
    apply_beta(InRest,A,B,OutRest).

values([0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]).
%select_weighted(ID, Weights, Values, Value, Rest)
b(X,A,B) :- 
    values(Values),
    apply_beta(Values,A,B,Weights),
    select_weighted(b(A,B), Weights, Values, X, _).

latent_prior(Prob, A, B) :- b(Prob, A, B).

% urn(NWhite, NBlack, NReplace)
urn(1,2,4).

PB::thunk(b,N); PW::thunk(w,N) :-
    urn(NWhite, NBlack, NReplace),
    A is NBlack/NReplace,
    B is NWhite/NReplace,
    latent_prior(P,A,B),
    PB is P, PW is 1.0-P.

% repeat(Thunk, N, Result). -> with Tail Recursion
repeat(Thunk,N,Result) :-
    repeat(Thunk,N,[],Result).
repeat(_,0,Acc,Acc).
repeat(Thunk, N, Acc, Result) :-
    N > 0,
    call(Thunk, Sample, N),
    N1 is N-1,
    repeat(Thunk, N1, [Sample|Acc], Result).

query(repeat(thunk, 3, [_,_,_])).
