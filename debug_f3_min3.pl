:- consult('f3_assert.pl').

debug_f3_min3 :-
    writeln('=== Testing the actual constraint ==='),
    X = 25,
    writeln('Testing: Z #= min(X+10, X+20) directly'),
    Z #= min(X+10, X+20),
    format('Z = ~w~n', [Z]),
    
    writeln('Now testing F3 equivalent with trace'),
    trace,
    user:p([min, [X, +, 10], [X, +, 20]], =, W),
    notrace,
    format('W = ~w~n', [W]).

:- debug_f3_min3.