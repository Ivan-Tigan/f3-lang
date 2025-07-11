:- use_module(library(clpfd)).
:- consult('f3_assert.pl').

debug_transform_detail :-
    writeln('=== Debugging transform_expr in detail ==='),
    X = 25,
    Input = [min, [X, +, 10], [X, +, 20]],
    format('Input: ~w~n', [Input]),
    
    % Let's manually trace what should happen
    writeln('Expected transformation:'),
    writeln('1. [min, [25, +, 10], [25, +, 20]]'),
    writeln('2. build_min_max(min, [[25, +, 10], [25, +, 20]], Result)'),
    writeln('3. transform_expr([25, +, 10], FirstExpr) -> 25+10'),
    writeln('4. transform_expr([25, +, 20], SecondExpr) -> 25+20'),
    writeln('5. Result = min(25+10, 25+20)'),
    
    % Test the actual transformation
    writeln('Testing actual F3 transformation:'),
    user:p([min, [X, +, 10], [X, +, 20]], =, Result),
    format('Actual result: ~w~n', [Result]).

:- debug_transform_detail.