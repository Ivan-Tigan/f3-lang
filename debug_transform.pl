:- use_module(library(clpfd)).
:- consult('f3_assert.pl').

% Let me try to call transform_expr directly by adding it to the module
debug_transform :-
    writeln('=== Debugging transform_expr ==='),
    % Since transform_expr is not exported, let's test the actual equality constraint
    X = 25,
    writeln('Testing: user:p([min, [X, +, 10], [X, +, 20]], =, W)'),
    user:p([min, [X, +, 10], [X, +, 20]], =, W),
    format('Result W = ~w~n', [W]),
    
    writeln('Now testing if W is actually a CLP(FD) constraint'),
    (   W = min(_, _) -> 
        format('W is a proper min term: ~w~n', [W])
    ;   format('W is NOT a proper min term, it is: ~w~n', [W])
    ).

:- debug_transform.