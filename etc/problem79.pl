% -*- Prolog -*-

before([X | Soln], X, Y) :-
    member(Y, Soln).
before([_ | Soln], Y, Z) :-
    before(Soln, Y, Z).

with_clues(Soln) :-
    length(Soln, 8),
    before(Soln, 1, 0),
    before(Soln, 1, 2),
    before(Soln, 1, 6),
    before(Soln, 1, 8),
    before(Soln, 1, 9),
    before(Soln, 2, 0),
    before(Soln, 2, 8),
    before(Soln, 2, 9),
    before(Soln, 3, 1),
    before(Soln, 3, 6),
    before(Soln, 3, 8),
    before(Soln, 6, 0),
    before(Soln, 6, 2),
    before(Soln, 6, 8),
    before(Soln, 6, 9),
    before(Soln, 7, 1),
    before(Soln, 7, 2),
    before(Soln, 7, 3),
    before(Soln, 7, 6),
    before(Soln, 7, 9),
    before(Soln, 8, 0),
    before(Soln, 8, 9),
    before(Soln, 9, 0),
    msort(Soln, Soln0),
    length(Soln0, 8).

:-
    with_clues(Soln),
    write(Soln),
    halt(0).
