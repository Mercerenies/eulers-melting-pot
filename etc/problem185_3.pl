% -*- Prolog -*-

% Also too slow :(

:- use_module(library(clpb)).

is_digit(0).
is_digit(1).
is_digit(2).
is_digit(3).
is_digit(4).
is_digit(5).
is_digit(6).
is_digit(7).
is_digit(8).
is_digit(9).

empty_diff(Xs-Xs).

concat_diff([], Ys-Ys).
concat_diff([Xs|Xss], Zs) :-
    concat_diff(Xss, Ys),
    app_diff(Xs, Ys, Zs).

app_diff(Xs-Ys, Ys-Zs, Xs-Zs).

realize_diff(Xs-[], Xs).

is_guess(guess(Digits, Correct)) :-
    maplist(is_digit, Digits),
    integer(Correct).

guess_length(guess(Digits, _), N) :-
    length(Digits, N).

all_vars_list([Guess|_], Varss) :-
    guess_length(Guess, AnswerLen),
    length(Varss, AnswerLen),
    length(Vars, 10),
    maplist(same_length(Vars), Varss).

one_digit_is_correct(Vars, [card([1], Vars)|Xs]-Xs).

one_digit_is_correct_each(Varss, Cs) :-
    maplist(one_digit_is_correct, Varss, Css),
    concat_diff(Css, Cs).

referenced_digits(Varss, Digits, OutputVars) :-
    maplist(nth0, Digits, Varss, OutputVars).

guess_constraint(Varss, guess(Digits, CorrectCount), [card([CorrectCount], RelevantVars)|Xs]-Xs) :-
    referenced_digits(Varss, Digits, RelevantVars).

guess_constraints(Varss, Guesses, Cs) :-
    maplist(guess_constraint(Varss), Guesses, Css),
    concat_diff(Css, Cs).

build_constraints(Varss, Guesses, Cs) :-
    one_digit_is_correct_each(Varss, Cs1),
    guess_constraints(Varss, Guesses, Cs2),
    app_diff(Cs1, Cs2, Cs_diff),
    realize_diff(Cs_diff, Cs).

recover_digit(Bits, Digit) :-
    nth0(Digit, Bits, 1),
    !.

solve(Guesses, Digits) :-
    all_vars_list(Guesses, Varss),
    build_constraints(Varss, Guesses, Cs),
    sat(*(Cs)),
    maplist(recover_digit, Varss, Digits).

short_guesses([
  guess([9, 0, 3, 4, 2], 2),
  guess([7, 0, 7, 9, 4], 0),
  guess([3, 9, 4, 5, 8], 2),
  guess([3, 4, 1, 0, 9], 1),
  guess([5, 1, 5, 4, 5], 2),
  guess([1, 2, 5, 3, 1], 1)
]).

long_guesses([
  guess([5, 6, 1, 6, 1, 8, 5, 6, 5, 0, 5, 1, 8, 2, 9, 3], 2),
  guess([3, 8, 4, 7, 4, 3, 9, 6, 4, 7, 2, 9, 3, 0, 4, 7], 1),
  guess([5, 8, 5, 5, 4, 6, 2, 9, 4, 0, 8, 1, 0, 5, 8, 7], 3),
  guess([9, 7, 4, 2, 8, 5, 5, 5, 0, 7, 0, 6, 8, 3, 5, 3], 3),
  guess([4, 2, 9, 6, 8, 4, 9, 6, 4, 3, 6, 0, 7, 5, 4, 3], 3),
  guess([3, 1, 7, 4, 2, 4, 8, 4, 3, 9, 4, 6, 5, 8, 5, 8], 1),
  guess([4, 5, 1, 3, 5, 5, 9, 0, 9, 4, 1, 4, 6, 1, 1, 7], 2),
  guess([7, 8, 9, 0, 9, 7, 1, 5, 4, 8, 9, 0, 8, 0, 6, 7], 3),
  guess([8, 1, 5, 7, 3, 5, 6, 3, 4, 4, 1, 1, 8, 4, 8, 3], 1),
  guess([2, 6, 1, 5, 2, 5, 0, 7, 4, 4, 3, 8, 6, 8, 9, 9], 2),
  guess([8, 6, 9, 0, 0, 9, 5, 8, 5, 1, 5, 2, 6, 2, 5, 4], 3),
  guess([6, 3, 7, 5, 7, 1, 1, 9, 1, 5, 0, 7, 7, 0, 5, 0], 1),
  guess([6, 9, 1, 3, 8, 5, 9, 1, 7, 3, 1, 2, 1, 3, 6, 0], 1),
  guess([6, 4, 4, 2, 8, 8, 9, 0, 5, 5, 0, 4, 2, 7, 6, 8], 2),
  guess([2, 3, 2, 1, 3, 8, 6, 1, 0, 4, 3, 0, 3, 8, 4, 5], 0),
  guess([2, 3, 2, 6, 5, 0, 9, 4, 7, 1, 2, 7, 1, 4, 4, 8], 2),
  guess([5, 2, 5, 1, 5, 8, 3, 3, 7, 9, 6, 4, 4, 3, 2, 2], 2),
  guess([1, 7, 4, 8, 2, 7, 0, 4, 7, 6, 7, 5, 8, 2, 7, 6], 3),
  guess([4, 8, 9, 5, 7, 2, 2, 6, 5, 2, 1, 9, 0, 3, 0, 6], 1),
  guess([3, 0, 4, 1, 6, 3, 1, 1, 1, 7, 2, 2, 4, 6, 3, 5], 3),
  guess([1, 8, 4, 1, 2, 3, 6, 4, 5, 4, 3, 2, 4, 5, 8, 9], 3),
  guess([2, 6, 5, 9, 8, 6, 2, 6, 3, 7, 3, 1, 6, 8, 6, 7], 2)
]).

:-
    long_guesses(Guesses),
    solve(Guesses, Varss),
    write(Varss),
    nl.
