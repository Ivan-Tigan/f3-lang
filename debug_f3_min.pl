:- consult('f3_assert.pl').

debug_f3_min :-
    writeln('=== Debugging F3 min transformation ==='),
    % Test what our transform_expr produces for min
    transform_expr([min, [20, +, 10], [20, +, 20]], Result),
    format('transform_expr([min, [20, +, 10], [20, +, 20]]) = ~w~n', [Result]),
    
    % Test with variables
    X = 20,
    transform_expr([min, [X, +, 10], [X, +, 20]], Result2),
    format('transform_expr([min, [X, +, 10], [X, +, 20]]) = ~w~n', [Result2]).

:- debug_f3_min.