:- consult('f3_assert.pl').

test_trace_problem :-
    writeln('=== Tracing the problem ==='),
    
    % Create a free variable
    X = _,
    format('X = ~w~n', [X]),
    
    % Show the structure we're trying to transform
    Structure = [X, +, 10],
    format('Structure = ~w~n', [Structure]),
    
    % Test if it matches the binary pattern
    ( Structure = [Left, Op, Right] ->
        format('Matches binary pattern: Left=~w, Op=~w, Right=~w~n', [Left, Op, Right])
    ;   writeln('Does not match binary pattern')
    ),
    
    % Test if it matches the min pattern
    ( Structure = [min | Values] ->
        format('Matches min pattern: Values=~w~n', [Values])
    ;   writeln('Does not match min pattern')
    ),
    
    % Now test the actual transformation
    writeln('Testing actual transformation:'),
    user:p(Structure, =, Result),
    format('Result = ~w~n', [Result]).

:- test_trace_problem.