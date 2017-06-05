%%% -*- Mode: Prolog; -*-
% 22/04/17

%  Tug of War

0.2::strength(Person, 0); 0.2::strength(Person, 1); 0.2::strength(Person, 2);0.2::strength(Person, 3);0.2::strength(Person, 4) :- person(Person).

1/3::lazy(Person, Game) :- person(Person), game(Game).

pulling([], _, 0).
pulling([Member|Team], Game, P) :-
    strength(Member, S), \+ lazy(Member, Game), % Team member is NOT lazy
    pulling(Team, Game, P1),
    P is P1 + S.
    
pulling([Member|Team], Game, P) :-
    strength(Member, S), lazy(Member, Game),    % Team member is NOT lazy
    pulling(Team, Game, P1),
    P is P1 + S/2.

win(Team1, Team2, Game) :-
    pulling(Team1, Game, P1),
    pulling(Team2, Game, P2),
    P1 >= P2.

game(game1). game(game2).
person(bob). person(mary). person(tom).
person(sue). person(jim).

evidence(win([bob, mary], [tom, sue], game1)).
evidence(win([bob, sue],  [tom, jim], game2)).

query(strength(bob, S)).
