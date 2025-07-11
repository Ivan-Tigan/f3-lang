:- consult('f3_assert.pl').

test_real_min :-
    writeln('=== Testing real min operation ==='),
    
    % Test with actual min
    writeln('Test 1: Real min operation'),
    user:p([min, 5, 10], =, Result1),
    format('Result1 = ~w~n', [Result1]),
    
    % Test with min and expressions
    writeln('Test 2: Min with expressions'),
    user:p([min, [3, +, 2], [7, +, 1]], =, Result2),
    format('Result2 = ~w~n', [Result2]).

:- test_real_min.