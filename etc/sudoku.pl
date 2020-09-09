% -*- Prolog -*-

% Messing with solving Sudoku using backward chaining, so I can do it
% for Problem 95.

:- use_module(library(clpfd)).

value(V) :- V in 1..9.

value_strong(1).
value_strong(2).
value_strong(3).
value_strong(4).
value_strong(5).
value_strong(6).
value_strong(7).
value_strong(8).
value_strong(9).

coord(c(X, Y)) :- value(X), value(Y).

coords(c(X, Y), X, Y) :- value(X), value(Y).

at(P, sudoku(Grid), V) :-
    coords(P, X, Y),
    I #= (Y - 1) * 9 + (X - 1),
    nth0(I, Grid, V).

row_ok(S, Y) :-
    findall(V, (value_strong(X), at(c(X, Y), S, V)), Vs),
    write(Vs),
    all_distinct(Vs).

sudoku(sudoku(Grid)) :-
    length(Grid, 9),
    maplist(value, Grid),
    forall(value_strong(Y), row_ok(sudoku(Grid), Y)).

sample(X) :-
    sudoku(X),
    X = sudoku(Tmp),
    label(Tmp).

:-
    sample(X),
    !,
    write(X),
    write('\n').
