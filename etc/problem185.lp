
% Doing it in Potassco for funsies :) (this is just the 5-digit variant)

guessnumber(0..5).
position(0..4).
digit(0..9).

correct(I, D) :-
  position(I),
  digit(D),
  not not correct(I, D).

correctinguess(0, 2).
correctinguess(1, 0).
correctinguess(2, 2).
correctinguess(3, 1).
correctinguess(4, 2).
correctinguess(5, 1).

% guess(guesscount, index, digit)

guess(0, 0, 9).
guess(0, 1, 0).
guess(0, 2, 3).
guess(0, 3, 4).
guess(0, 4, 2).

guess(1, 0, 7).
guess(1, 1, 0).
guess(1, 2, 7).
guess(1, 3, 9).
guess(1, 4, 4).

guess(2, 0, 3).
guess(2, 1, 9).
guess(2, 2, 4).
guess(2, 3, 5).
guess(2, 4, 8).

guess(3, 0, 3).
guess(3, 1, 4).
guess(3, 2, 1).
guess(3, 3, 0).
guess(3, 4, 9).

guess(4, 0, 5).
guess(4, 1, 1).
guess(4, 2, 5).
guess(4, 3, 4).
guess(4, 4, 5).

guess(5, 0, 1).
guess(5, 1, 2).
guess(5, 2, 5).
guess(5, 3, 3).
guess(5, 4, 1).

iscorrect(N, I) :- correct(I, D), guess(N, I, D).

{ correct(I, D) : digit(D) } = 1 :- position(I).

{ iscorrect(N, I) : position(I) } = X :-
  guessnumber(N),
  correctinguess(N, X).

:- iscorrect(N, I), correct(I, D1), guess(N, I, D2), D1 != D2.

#show correct/2.
