:- consult('f3_assert.pl').

debug_f3_min2 :-
    writeln('=== Debugging F3 min step by step ==='),
    X = 25,
    writeln('Step 1: Testing simple min evaluation'),
    user:p([min, [X, +, 10], [X, +, 20]], =, Z),
    format('min([25+10], [25+20]) = ~w~n', [Z]),
    
    writeln('Step 2: Testing with CLP(FD) variable'),
    user:p(Y, in, [20, 30]),
    user:p([min, [Y, +, 10], [Y, +, 20]], =, W),
    format('min([Y+10], [Y+20]) where Y in [20,30] = ~w~n', [W]).

:- debug_f3_min2.