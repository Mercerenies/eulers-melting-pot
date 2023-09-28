
% Probably a working solution (in the sense that it will enumerate all
% valid models), but far far too slow to ever work.
%
% The `t` predicate is simply something to print. Since we only want
% to count solutions, I wanted to print the bare minimum necessary, to
% save on space.

row(1..4).
column(1..4).
digit(0..9).

position(cell(R, C)) :- row(R), column(C).

summation(row(1..4)).
summation(column(1..4)).
summation(nw_se_diagonal).
summation(ne_sw_diagonal).

constituent(row(R), cell(R, C)) :- row(R), column(C).
constituent(column(C), cell(R, C)) :- row(R), column(C).
constituent(nw_se_diagonal, cell(X, X)) :- row(X), column(X).
constituent(ne_sw_diagonal, cell(X, Y)) :- row(X), column(Y), X + Y = 5.

sum(S, N) :- summation(S), N = #sum { K : constituent(S, P), at_cell(P, K) }.

at_cell(P, D) :- position(P), digit(D), not not at_cell(P, D).

{ at_cell(P, D) : digit(D) } = 1 :- position(P).

:- sum(_, N1), sum(_, N2), N1 != N2.

t.

#show t/0.
