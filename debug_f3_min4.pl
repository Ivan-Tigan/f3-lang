:- use_module(library(clpfd)).
:- consult('f3_assert.pl').

debug_f3_min4 :-
    writeln('=== Testing the actual constraint ==='),
    X = 25,
    writeln('Testing: Z #= min(X+10, X+20) directly'),
    Z #= min(X+10, X+20),
    format('Z = ~w~n', [Z]),
    
    writeln('Now testing what our transform should produce'),
    % What we want: min(25+10, 25+20)
    Expected = min(25+10, 25+20),
    format('Expected CLP(FD) expression: ~w~n', [Expected]),
    
    writeln('Testing F3 version'),
    user:p([min, [X, +, 10], [X, +, 20]], =, W),
    format('F3 result: ~w~n', [W]).

:- debug_f3_min4.