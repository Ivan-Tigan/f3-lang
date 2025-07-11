:- consult('f3_assert.pl').

test_min :-
    writeln('Testing min constraint:'),
    user:p(X, in, [20, 60]),
    user:p(Y, >, 5),
    format('Testing: [3 + [7 * Y]] = [min [X + 10] [X + 20]]~n'),
    user:p([3, +, [7, *, Y]], =, [min, [X, +, 10], [X, +, 20]]),
    format('X = ~w, Y = ~w~n', [X, Y]),
    user:p([Y, X], labels, AllSolutions),
    format('AllSolutions = ~w~n', [AllSolutions]).

:- test_min.