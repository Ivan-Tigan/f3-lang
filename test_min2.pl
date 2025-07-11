:- consult('f3_assert.pl').

test_min2 :-
    writeln('Testing simpler min:'),
    user:p(X, in, [20, 30]),
    format('X = ~w~n', [X]),
    user:p([min, [X, +, 10], [X, +, 20]], =, Result),
    format('min([X+10], [X+20]) = ~w~n', [Result]).

:- test_min2.