% Probabilistic Context-Free Grammars

% 1. Terminal
0.5::d([the], ID); 0.5::d([a], ID).
1/3::n([chef], ID); 1/3::n([soup], ID);1/3::n([omelet], ID).
0.5::v([cooks], ID); 0.5::v([works], ID).
a([diligently], ID).

% 2. Non-Terminal
ap(AP, id(ID)) :-
    a(AP, id([left|ID])).
np(NP, id(ID)) :-
    d(D, id([left|ID])), n(N, id([left|ID])),
    append(D,N,NP).
0.5::vp1; 0.5::vp2. % choose between vp -> v ap or vp -> v np
vp(VP, id(ID)) :-
    vp1,
    v(V, id([left|ID])), ap(AP, id([right|ID])),
    append(V, AP,VP).
vp(VP, id(ID)) :-
    vp2,
    v(V,id([left|ID])), np(NP, id([right|ID])),
    append(V,NP,VP).
s(S) :-
    np(NP, id([left])), vp(VP, id([right])),
    append(NP,VP,S).

% auxiliary
append([],L,L).
append([H|T], L1, [H|L2]) :-
    append(T, L1, L2).

query(s(S)).
