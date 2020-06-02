
info(3, 1, 9).
info(6, 8, 0).
info(1, 8, 0).
info(6, 9, 0).
info(1, 2, 9).
info(6, 2, 0).
info(7, 6, 2).
info(6, 8, 9).
info(7, 6, 2).
info(3, 1, 8).
info(3, 6, 8).
info(7, 1, 0).
info(7, 2, 0).
info(7, 1, 0).
info(6, 2, 9).
info(1, 6, 8).
info(1, 6, 0).
info(6, 8, 9).
info(7, 1, 6).
info(7, 3, 1).
info(7, 3, 6).
info(7, 2, 9).
info(3, 1, 6).
info(7, 2, 9).
info(7, 2, 9).
info(7, 1, 0).
info(7, 6, 9).
info(2, 9, 0).
info(7, 1, 9).
info(6, 8, 0).
info(3, 1, 8).
info(3, 8, 9).
info(1, 6, 2).
info(2, 8, 9).
info(1, 6, 2).
info(7, 1, 8).
info(7, 2, 9).
info(3, 1, 9).
info(7, 9, 0).
info(6, 8, 0).
info(8, 9, 0).
info(3, 6, 2).
info(3, 1, 9).
info(7, 6, 0).
info(3, 1, 6).
info(7, 2, 9).
info(3, 8, 0).
info(3, 1, 9).
info(7, 2, 8).
info(7, 1, 6).

isnum(0;1;2;3;6;7;8;9).

ispos(1..8).

isclue(clue(A, B)) :- info(A, B, _).
isclue(clue(B, C)) :- info(_, B, C).

pos(N, X) :- isnum(X), ispos(N), not not pos(N, X).

{ pos(N, X) : isnum(X), ispos(N) } = 8.

:- pos(N, X), pos(M, X), N != M.
:- pos(N, X), pos(N, Y), X != Y.
:- pos(N, X), pos(M, Y), N < M, isclue(clue(Y, X)).

#show.
#show pos/2.
