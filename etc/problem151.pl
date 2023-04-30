% -*- Prolog -*-

% I have a working Kotlin implementation of this. But just for fun, I
% want to do it in Prolog too ^.^

:- dynamic consume/3.

% cutup(+CurrentPaper, +TargetSize, -ResultingPapers)
cutup(CurrentPaper, CurrentPaper, []).
cutup(CurrentPaper, TargetSize, ResultingPapers) :-
    M is CurrentPaper + 1,
    numlist(M, TargetSize, ResultingPapers).

% removed(?Xs, ?Z, ?Ys) - Ys is Xs without one copy of Z.
removed([X|Xs], X, Xs).
removed([X|Xs], Z, [X|Ys]) :-
    removed(Xs, Z, Ys).

% get_of_size(+OldEnvelope, +TargetSize, -NewEnvelope)
get_of_size(OldEnvelope, TargetSize, NewEnvelope) :-
    removed(OldEnvelope, Paper, Envelope1),
    cutup(Paper, TargetSize, NewPapers),
    append(Envelope1, NewPapers, NewEnvelope).

% currently_size_1(+Envelope, -ExpectedValueModifier)
currently_size_1([_], 1) :-
    !.
currently_size_1(_, 0).

% consume_helper(+Envelope, +TargetSize, -ExpectedValue)
consume_helper(Envelope, TargetSize, ExpectedValue) :-
    get_of_size(Envelope, TargetSize, NewEnvelope),
    once(consume(NewEnvelope, TargetSize, ExpectedValue)).

% div(+A, +B, -C)
%
% Division with arguments reversed.
div(A, B, C) :-
    C is B / A.

% consume(+Envelope, +TargetSize, -ExpectedValue)
consume([], _, 0) :-
    !.
consume(Envelope, TargetSize, ExpectedValue) :-
    currently_size_1(Envelope, ExpectedValueModifier),
    findall(
        E,
        consume_helper(Envelope, TargetSize, E),
        Es
    ),
    length(Envelope, L),
    maplist(div(L), Es, Es1),
    sum_list(Es1, BaseExpectedValue),
    ExpectedValue is BaseExpectedValue + ExpectedValueModifier,
    asserta(consume(Envelope, TargetSize, ExpectedValue)).

:-
    consume([1], 5, E),
    E1 is E - 2,
    write(E1).
