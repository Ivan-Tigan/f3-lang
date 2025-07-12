:- module(f3_clpqr, []).
:- use_module(library(clpq)).
:- multifile user:p/3.

% CLP(QR) builtin for F3 - Constraint Logic Programming over Rationals/Reals
% Transform F3 arrays into CLP(QR) constraints and expressions

% DCG grammar for parsing constraint arrays (similar to clpqr_test.pl)
constraints(Constraints) -->
    constraint_expr(First),
    constraint_rest(First, Constraints).

constraint_rest(First, (First, Rest)) -->
    [and],
    constraint_expr(Second),
    constraint_rest(Second, Rest).
constraint_rest(First, (First, Rest)) -->
    [&],
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

% Handle nested expression arrays like [Y1, +, Y2]
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
        ;   atom(X), \+ member(X, [+, -, *, /, =<, >=, =, <, >, =:=, =\=, and, &])
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

% Transform F3 constraint array to CLP(QR) constraints using DCG
transform_constraints(List, Constraints) :-
    phrase(constraints(Constraints), List).

% Transform expression arrays to CLP(QR) expressions (for minimize/maximize)
transform_expr(Expr, Result) :-
    var(Expr), !,
    Result = Expr.

transform_expr(Expr, Result) :-
    number(Expr), !,
    Result = Expr.

transform_expr(Expr, Result) :-
    atom(Expr), !,
    Result = Expr.

% Handle nested arrays like [Y1, +, Y2] -> Y1+Y2
transform_expr([Left, Op, Right], Result) :-
    !,
    transform_expr(Left, LeftExpr),
    transform_expr(Right, RightExpr),
    binary_op(Op, LeftExpr, RightExpr, Result).

% Single element array
transform_expr([Expr], Result) :-
    !, transform_expr(Expr, Result).

% Fallback
transform_expr(Expr, Expr).

% Binary operations
binary_op(+, L, R, L + R).
binary_op(-, L, R, L - R).
binary_op(*, L, R, L * R).
binary_op(/, L, R, L / R).

% Main CLP(QR) constraint predicate
% clpqr constraint [constraints]
user:p(clpqr, constraint, ConstraintArray) :-
    transform_constraints(ConstraintArray, Constraints),
    apply_constraints(Constraints).

% Apply constraints using CLP(QR) - handle constraint structures
apply_constraints(Constraints) :-
    { Constraints }.

% Minimize expression
% clpqr minimize [expression]
user:p(clpqr, minimize, ExprArray) :-
    transform_expr(ExprArray, Expr),
    minimize(Expr).

% Maximize expression  
% clpqr maximize [expression]
user:p(clpqr, maximize, ExprArray) :-
    transform_expr(ExprArray, Expr),
    maximize(Expr).

% Test predicate
test_clpqr :-
    % Test the exact case from the requirements - direct approach
    write('Testing CLP(QR) implementation...'), nl,
    {
        X1 =< 50,
        X2 =< 200,
        X1 + 0.2 * X2 =< 72,
        150 * X1 + 25 * X2 =< 10000,
        Z = 250 * X1 + 45 * X2,
        2 * (Y1 + Y2) = X1,
        Y2 = 10
    },
    maximize(Z + 5),
    format('X1: ~w, X2: ~w, Z: ~w, Y1: ~w, Y2: ~w~n', [X1, X2, Z, Y1, Y2]).

% Test with array transformation via user:p
test_clpqr_array :-
    write('Testing CLP(QR) with user:p interface...'), nl,
    % Test the actual case using user:p interface
    user:p(clpqr, constraint, [
        X1, =<, 50, and,
        X2, =<, 200, and,
        X1, +, 0.2, *, X2, =<, 72, and,
        150, *, X1, +, 25, *, X2, =<, 10000, and,
        Z, =, 250, *, X1, +, 45, *, X2, and,
        2, *, [Y1, +, Y2], =, X1
    ]),
    user:p(clpqr, maximize, [Z, +, 5]),
    format('X1: ~w, X2: ~w, Z: ~w, Y1: ~w, Y2: ~w~n', [X1, X2, Z, Y1, Y2]).

% Test matching the F3 example (without Y2=10 constraint)
test_clpqr_f3_case :-
    write('Testing CLP(QR) F3 case...'), nl,
    user:p(clpqr, constraint, [
        X1, +, 0.2, *, X2, =<, 72, and,
        150, *, X1, +, 25, *, X2, =<, 10000, and,
        Z, =, 250, *, X1, +, 45, *, X2, and,
        2, *, [Y1, +, Y2], =, X1 
    ]),
    user:p(clpqr, maximize, [Z, +, 5]),
    format('X1: ~w, X2: ~w, Z: ~w, Y1: ~w, Y2: ~w~n', [X1, X2, Z, Y1, Y2]).

% Test minimize operation
test_clpqr_minimize :-
    write('Testing CLP(QR) minimize...'), nl,
    user:p(clpqr, constraint, [
        X1, >=, 0, and,
        X2, >=, 0, and,
        X1, +, 0.2, *, X2, =<, 72, and,
        150, *, X1, +, 25, *, X2, =<, 10000, and,
        Z, =, 250, *, X1, +, 45, *, X2, and,
        2, *, [Y1, +, Y2], =, X1 
    ]),
    user:p(clpqr, minimize, [Z, +, 5]),
    format('X1: ~w, X2: ~w, Z: ~w, Y1: ~w, Y2: ~w~n', [X1, X2, Z, Y1, Y2]).

% Test using & instead of and
test_clpqr_ampersand :-
    write('Testing CLP(QR) with & operator...'), nl,
    user:p(clpqr, constraint, [
        X1, +, 0.2, *, X2, =<, 72, &,
        150, *, X1, +, 25, *, X2, =<, 10000, &,
        Z, =, 250, *, X1, +, 45, *, X2, &,
        2, *, [Y1, +, Y2], =, X1 
    ]),
    user:p(clpqr, maximize, [Z, +, 5]),
    format('X1: ~w, X2: ~w, Z: ~w, Y1: ~w, Y2: ~w~n', [X1, X2, Z, Y1, Y2]).

