:- consult('f3_assert.pl').

test_labels :-
    writeln('Testing clpfd labels:'),
    user:p(X, in, [1, 3]),
    format('X in [1,3], calling clpfd labels [X]~n'),
    user:p(clpfd, labels, [X]),
    format('After clpfd labels, X = ~w~n', [X]),
    writeln('clpfd labels completed').

:- test_labels.