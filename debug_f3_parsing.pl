:- consult('f3_assert.pl').

debug_f3_parsing :-
    writeln('=== Testing F3 parsing issue ==='),
    % Let's see what the F3 parser is actually producing
    writeln('Testing: [min [?X + 10] [?X + 20]]'),
    
    % First let's see what X looks like when parsed
    writeln('Step 1: Test simple variable'),
    user:p(X, in, [20, 60]),
    format('X = ~w~n', [X]),
    
    % Now test the min expression
    writeln('Step 2: Test min expression'),
    user:p([min, [X, +, 10], [X, +, 20]], =, Result),
    format('Result = ~w~n', [Result]).

:- debug_f3_parsing.