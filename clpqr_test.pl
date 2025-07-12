:- use_module(library(clpq)).

main :- 
    constraint([
        X1, +, 0.2, *, X2, =<, 72,
        and, 150, *, X1, +, 25, *, X2, =<, 10000,
        and, Z, =, 250, *, X1, +, 45, *, X2,
        and, 2, *, [Y1, +, Y2], =, X1
    ]),
    maximize(Z + 5),
    format('X1: ~w, X2: ~w, Z: ~w, Y1: ~w, Y2: ~w~n', [X1, X2, Z, Y1, Y2]).

% DCG grammar for parsing multiple constraints with nested expressions
constraints(Constraints) -->
    constraint_expr(First),
    constraint_rest(First, Constraints).

constraint_rest(First, (First, Rest)) -->
    [and],
    constraint_expr(Second),
    constraint_rest(Second, Rest).
constraint_rest(Constraint, Constraint) --> [].

constraint_expr(Constraint) -->
    expression(Left),
    comparison_op(Op),
    expression(Right),
    { Constraint =.. [Op, Left, Right] }.

expression(Result) -->
    term(Left),
    add_expr(Left, Result).

add_expr(Left, Result) -->
    add_op(Op),
    term(Right),
    { Temp =.. [Op, Left, Right] },
    add_expr(Temp, Result).
add_expr(Result, Result) --> [].

term(Result) -->
    factor(Left),
    mul_expr(Left, Result).

mul_expr(Left, Result) -->
    mul_op(Op),
    factor(Right),
    { Temp =.. [Op, Left, Right] },
    mul_expr(Temp, Result).
mul_expr(Result, Result) --> [].

% Updated factor rule to handle nested expression arrays
factor(Result) -->
    [List], { 
        is_list(List) 
    },
    { phrase(expression(Result), List) }.

factor(X) -->
    [X], { 
        \+ is_list(X),
        (   var(X) -> true
        ;   number(X) -> true  
        ;   atom(X), \+ member(X, [+, -, *, /, =<, >=, =, <, >, =:=, =\=, and])
        )
    }.

add_op(+) --> [+].
add_op(-) --> [-].

mul_op(*) --> [*].
mul_op(/) --> [/].

comparison_op(=<) --> [=<].
comparison_op(>=) --> [>=].
comparison_op(=) --> [=].
comparison_op(<) --> [<].
comparison_op(>) --> [>].
comparison_op(=:=) --> [=:=].
comparison_op(=\=) --> [=\=].

constraint(List) :-
    phrase(constraints(Constraints), List),
    call({Constraints}).