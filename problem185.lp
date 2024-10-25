
% Doing it in Potassco for funsies :) (16-digit variant)

guessnumber(0..21).
position(0..15).
digit(0..9).

correct(I, D) :-
  position(I),
  digit(D),
  not not correct(I, D).

correctinguess(0, 2).
correctinguess(1, 1).
correctinguess(2, 3).
correctinguess(3, 3).
correctinguess(4, 3).
correctinguess(5, 1).
correctinguess(6, 2).
correctinguess(7, 3).
correctinguess(8, 1).
correctinguess(9, 2).
correctinguess(10, 3).
correctinguess(11, 1).
correctinguess(12, 1).
correctinguess(13, 2).
correctinguess(14, 0).
correctinguess(15, 2).
correctinguess(16, 2).
correctinguess(17, 3).
correctinguess(18, 1).
correctinguess(19, 3).
correctinguess(20, 3).
correctinguess(21, 2).

% guess(guesscount, index, digit)
%
% Whee! Perl is fun:
% $ cat /tmp/a.txt | perl -lanE 's/ ;.*//; $i=0; for (/./g) { say "guess(@{[$.-1]}, $i, $_)."; $i++ }'
guess(0, 0, 5).
guess(0, 1, 6).
guess(0, 2, 1).
guess(0, 3, 6).
guess(0, 4, 1).
guess(0, 5, 8).
guess(0, 6, 5).
guess(0, 7, 6).
guess(0, 8, 5).
guess(0, 9, 0).
guess(0, 10, 5).
guess(0, 11, 1).
guess(0, 12, 8).
guess(0, 13, 2).
guess(0, 14, 9).
guess(0, 15, 3).
guess(1, 0, 3).
guess(1, 1, 8).
guess(1, 2, 4).
guess(1, 3, 7).
guess(1, 4, 4).
guess(1, 5, 3).
guess(1, 6, 9).
guess(1, 7, 6).
guess(1, 8, 4).
guess(1, 9, 7).
guess(1, 10, 2).
guess(1, 11, 9).
guess(1, 12, 3).
guess(1, 13, 0).
guess(1, 14, 4).
guess(1, 15, 7).
guess(2, 0, 5).
guess(2, 1, 8).
guess(2, 2, 5).
guess(2, 3, 5).
guess(2, 4, 4).
guess(2, 5, 6).
guess(2, 6, 2).
guess(2, 7, 9).
guess(2, 8, 4).
guess(2, 9, 0).
guess(2, 10, 8).
guess(2, 11, 1).
guess(2, 12, 0).
guess(2, 13, 5).
guess(2, 14, 8).
guess(2, 15, 7).
guess(3, 0, 9).
guess(3, 1, 7).
guess(3, 2, 4).
guess(3, 3, 2).
guess(3, 4, 8).
guess(3, 5, 5).
guess(3, 6, 5).
guess(3, 7, 5).
guess(3, 8, 0).
guess(3, 9, 7).
guess(3, 10, 0).
guess(3, 11, 6).
guess(3, 12, 8).
guess(3, 13, 3).
guess(3, 14, 5).
guess(3, 15, 3).
guess(4, 0, 4).
guess(4, 1, 2).
guess(4, 2, 9).
guess(4, 3, 6).
guess(4, 4, 8).
guess(4, 5, 4).
guess(4, 6, 9).
guess(4, 7, 6).
guess(4, 8, 4).
guess(4, 9, 3).
guess(4, 10, 6).
guess(4, 11, 0).
guess(4, 12, 7).
guess(4, 13, 5).
guess(4, 14, 4).
guess(4, 15, 3).
guess(5, 0, 3).
guess(5, 1, 1).
guess(5, 2, 7).
guess(5, 3, 4).
guess(5, 4, 2).
guess(5, 5, 4).
guess(5, 6, 8).
guess(5, 7, 4).
guess(5, 8, 3).
guess(5, 9, 9).
guess(5, 10, 4).
guess(5, 11, 6).
guess(5, 12, 5).
guess(5, 13, 8).
guess(5, 14, 5).
guess(5, 15, 8).
guess(6, 0, 4).
guess(6, 1, 5).
guess(6, 2, 1).
guess(6, 3, 3).
guess(6, 4, 5).
guess(6, 5, 5).
guess(6, 6, 9).
guess(6, 7, 0).
guess(6, 8, 9).
guess(6, 9, 4).
guess(6, 10, 1).
guess(6, 11, 4).
guess(6, 12, 6).
guess(6, 13, 1).
guess(6, 14, 1).
guess(6, 15, 7).
guess(7, 0, 7).
guess(7, 1, 8).
guess(7, 2, 9).
guess(7, 3, 0).
guess(7, 4, 9).
guess(7, 5, 7).
guess(7, 6, 1).
guess(7, 7, 5).
guess(7, 8, 4).
guess(7, 9, 8).
guess(7, 10, 9).
guess(7, 11, 0).
guess(7, 12, 8).
guess(7, 13, 0).
guess(7, 14, 6).
guess(7, 15, 7).
guess(8, 0, 8).
guess(8, 1, 1).
guess(8, 2, 5).
guess(8, 3, 7).
guess(8, 4, 3).
guess(8, 5, 5).
guess(8, 6, 6).
guess(8, 7, 3).
guess(8, 8, 4).
guess(8, 9, 4).
guess(8, 10, 1).
guess(8, 11, 1).
guess(8, 12, 8).
guess(8, 13, 4).
guess(8, 14, 8).
guess(8, 15, 3).
guess(9, 0, 2).
guess(9, 1, 6).
guess(9, 2, 1).
guess(9, 3, 5).
guess(9, 4, 2).
guess(9, 5, 5).
guess(9, 6, 0).
guess(9, 7, 7).
guess(9, 8, 4).
guess(9, 9, 4).
guess(9, 10, 3).
guess(9, 11, 8).
guess(9, 12, 6).
guess(9, 13, 8).
guess(9, 14, 9).
guess(9, 15, 9).
guess(10, 0, 8).
guess(10, 1, 6).
guess(10, 2, 9).
guess(10, 3, 0).
guess(10, 4, 0).
guess(10, 5, 9).
guess(10, 6, 5).
guess(10, 7, 8).
guess(10, 8, 5).
guess(10, 9, 1).
guess(10, 10, 5).
guess(10, 11, 2).
guess(10, 12, 6).
guess(10, 13, 2).
guess(10, 14, 5).
guess(10, 15, 4).
guess(11, 0, 6).
guess(11, 1, 3).
guess(11, 2, 7).
guess(11, 3, 5).
guess(11, 4, 7).
guess(11, 5, 1).
guess(11, 6, 1).
guess(11, 7, 9).
guess(11, 8, 1).
guess(11, 9, 5).
guess(11, 10, 0).
guess(11, 11, 7).
guess(11, 12, 7).
guess(11, 13, 0).
guess(11, 14, 5).
guess(11, 15, 0).
guess(12, 0, 6).
guess(12, 1, 9).
guess(12, 2, 1).
guess(12, 3, 3).
guess(12, 4, 8).
guess(12, 5, 5).
guess(12, 6, 9).
guess(12, 7, 1).
guess(12, 8, 7).
guess(12, 9, 3).
guess(12, 10, 1).
guess(12, 11, 2).
guess(12, 12, 1).
guess(12, 13, 3).
guess(12, 14, 6).
guess(12, 15, 0).
guess(13, 0, 6).
guess(13, 1, 4).
guess(13, 2, 4).
guess(13, 3, 2).
guess(13, 4, 8).
guess(13, 5, 8).
guess(13, 6, 9).
guess(13, 7, 0).
guess(13, 8, 5).
guess(13, 9, 5).
guess(13, 10, 0).
guess(13, 11, 4).
guess(13, 12, 2).
guess(13, 13, 7).
guess(13, 14, 6).
guess(13, 15, 8).
guess(14, 0, 2).
guess(14, 1, 3).
guess(14, 2, 2).
guess(14, 3, 1).
guess(14, 4, 3).
guess(14, 5, 8).
guess(14, 6, 6).
guess(14, 7, 1).
guess(14, 8, 0).
guess(14, 9, 4).
guess(14, 10, 3).
guess(14, 11, 0).
guess(14, 12, 3).
guess(14, 13, 8).
guess(14, 14, 4).
guess(14, 15, 5).
guess(15, 0, 2).
guess(15, 1, 3).
guess(15, 2, 2).
guess(15, 3, 6).
guess(15, 4, 5).
guess(15, 5, 0).
guess(15, 6, 9).
guess(15, 7, 4).
guess(15, 8, 7).
guess(15, 9, 1).
guess(15, 10, 2).
guess(15, 11, 7).
guess(15, 12, 1).
guess(15, 13, 4).
guess(15, 14, 4).
guess(15, 15, 8).
guess(16, 0, 5).
guess(16, 1, 2).
guess(16, 2, 5).
guess(16, 3, 1).
guess(16, 4, 5).
guess(16, 5, 8).
guess(16, 6, 3).
guess(16, 7, 3).
guess(16, 8, 7).
guess(16, 9, 9).
guess(16, 10, 6).
guess(16, 11, 4).
guess(16, 12, 4).
guess(16, 13, 3).
guess(16, 14, 2).
guess(16, 15, 2).
guess(17, 0, 1).
guess(17, 1, 7).
guess(17, 2, 4).
guess(17, 3, 8).
guess(17, 4, 2).
guess(17, 5, 7).
guess(17, 6, 0).
guess(17, 7, 4).
guess(17, 8, 7).
guess(17, 9, 6).
guess(17, 10, 7).
guess(17, 11, 5).
guess(17, 12, 8).
guess(17, 13, 2).
guess(17, 14, 7).
guess(17, 15, 6).
guess(18, 0, 4).
guess(18, 1, 8).
guess(18, 2, 9).
guess(18, 3, 5).
guess(18, 4, 7).
guess(18, 5, 2).
guess(18, 6, 2).
guess(18, 7, 6).
guess(18, 8, 5).
guess(18, 9, 2).
guess(18, 10, 1).
guess(18, 11, 9).
guess(18, 12, 0).
guess(18, 13, 3).
guess(18, 14, 0).
guess(18, 15, 6).
guess(19, 0, 3).
guess(19, 1, 0).
guess(19, 2, 4).
guess(19, 3, 1).
guess(19, 4, 6).
guess(19, 5, 3).
guess(19, 6, 1).
guess(19, 7, 1).
guess(19, 8, 1).
guess(19, 9, 7).
guess(19, 10, 2).
guess(19, 11, 2).
guess(19, 12, 4).
guess(19, 13, 6).
guess(19, 14, 3).
guess(19, 15, 5).
guess(20, 0, 1).
guess(20, 1, 8).
guess(20, 2, 4).
guess(20, 3, 1).
guess(20, 4, 2).
guess(20, 5, 3).
guess(20, 6, 6).
guess(20, 7, 4).
guess(20, 8, 5).
guess(20, 9, 4).
guess(20, 10, 3).
guess(20, 11, 2).
guess(20, 12, 4).
guess(20, 13, 5).
guess(20, 14, 8).
guess(20, 15, 9).
guess(21, 0, 2).
guess(21, 1, 6).
guess(21, 2, 5).
guess(21, 3, 9).
guess(21, 4, 8).
guess(21, 5, 6).
guess(21, 6, 2).
guess(21, 7, 6).
guess(21, 8, 3).
guess(21, 9, 7).
guess(21, 10, 3).
guess(21, 11, 1).
guess(21, 12, 6).
guess(21, 13, 8).
guess(21, 14, 6).
guess(21, 15, 7).


iscorrect(N, I) :- correct(I, D), guess(N, I, D).

{ correct(I, D) : digit(D) } = 1 :- position(I).

{ iscorrect(N, I) : position(I) } = X :-
  guessnumber(N),
  correctinguess(N, X).

:- iscorrect(N, I), correct(I, D1), guess(N, I, D2), D1 != D2.

#show correct/2.
