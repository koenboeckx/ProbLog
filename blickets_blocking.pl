% Of Blickets and Blocking
0.2::blicket(B) :- block(B).

power(B, 0.9)  :-    blicket(B). % prob of power if bock is blicket: (define (power block) (if (blicket block) 0.9 0.05))
power(B, 0.05) :- \+ blicket(B). % prob of power otherwise

machine([Block|Blocks]) :-
    power(Block, P),
    flip(Block, P). % IMPORTANT: include Block in flip/2!
machine([Block|Blocks]) :-
    power(Block, P),
    \+flip(Block, P),
    machine(Blocks).
machine([]) :-
    flip(0, 0.05).
P::flip(Block, P).

block(a). block(b). block(c). block(d).

query(blicket(a)).
evidence(machine([a,b]), true).
