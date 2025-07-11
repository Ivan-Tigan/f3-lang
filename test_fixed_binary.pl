:- consult('f3_assert.pl').

test_fixed_binary :-
    writeln('=== Testing fixed binary operation ==='),
    
    % Test with fixed values first
    writeln('Test 1: Fixed values'),
    user:p([5, +, 10], =, Result1),
    format('Result1 = ~w~n', [Result1]).

:- test_fixed_binary.