:- consult('f3_assert.pl').

test_f3_min :-
    writeln('=== Testing F3 min implementation ==='),
    user:p(X, in, [20, 30]),
    user:p(Y, >, 5),
    user:p([3, +, [7, *, Y]], =, [min, [X, +, 10], [X, +, 20]]),
    format('X = ~w, Y = ~w~n', [X, Y]),
    user:p([Y, X], labels, AllSolutions),
    format('AllSolutions = ~w~n', [AllSolutions]).

:- test_f3_min.