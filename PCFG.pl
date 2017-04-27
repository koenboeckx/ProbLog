% Probabilistic Context-Free Grammars
% https://dtai.cs.kuleuven.be/problog/editor.html#task=sample&hash=a60ff4472bbe653b6fa9c7e8537f38f9

% Sampling only: (add identifiers to correctly handling normal inference)
% PCFG
% 1. Terminal
0.5::d([the]); 0.5::d([a]).
1/3::n([chef]); 1/3::n([soup]);1/3::n([omelet]).
0.5::v([cooks]); 0.5::v([works]).
a([diligently]).

% 2. Non-Terminal
ap(AP) :-
    a(AP).
np(NP) :-
    d(D), n(N),
    append(D,N,NP).
0.5::vp1; 0.5::vp2. % choose between vp -> v ap or vp -> v np
vp(VP) :-
    vp1,
    v(V), ap(AP),
    append(V, AP,VP).
vp(VP) :-
    vp2,
    v(V), np(NP),
    append(V,NP,VP).
s(S) :-
    np(NP), vp(VP),
    append(NP,VP,S).

% auxiliary
append([],L,L).
append([H|T], L1, [H|L2]) :-
    append(T, L1, L2).

query(s(S)).
