:- consult('f3_assert.pl').

test_simple_binary :-
    writeln('=== Testing simple binary operation ==='),
    
    % First test with regular variables
    writeln('Test 1: Regular variables'),
    user:p([X, +, 10], =, Result1),
    format('Result1 = ~w~n', [Result1]),
    
    % Test with CLP(FD) constrained variable
    writeln('Test 2: CLP(FD) variable'),
    user:p(Y, in, [1, 10]),
    user:p([Y, +, 10], =, Result2),
    format('Result2 = ~w~n', [Result2]).

:- test_simple_binary.