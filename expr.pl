:- use_module(library(clpr)).

% Expression evaluator for list-based syntax
eval_list(Num, Num) :- number(Num), !.
eval_list(Var, Var) :- var(Var), !.  
eval_list([Left, Op, Right], Result) :-
    eval_list(Left, L),
    eval_list(Right, R),
    make_constraint(Op, L, R, Result).

% Constraints using CLP(R)
make_constraint(+, L, R, Result) :- {Result = L + R}.
make_constraint(-, L, R, Result) :- {Result = L - R}.
make_constraint(*, L, R, Result) :- {Result = L * R}.
make_constraint(/, L, R, Result) :- {Result = L / R}.

% For use in f3 rules:
b(Expr, eval, Result) :-
    eval_list(Expr, Result).

% Tests
test :-
    writeln('Testing with different variable bindings:'),
    
    nl, writeln('1. Forward evaluation - all values known:'),
    test_expr([3, +, [4, *, 2]]),
    test_expr([10, -, [2, *, 3]]),
    
    nl, writeln('2. Forward evaluation - with variables:'),
    {A = 5},
    test_expr([A, +, [4, *, 2]]),
    
    nl, writeln('3. Simple variable solving:'),
    X1 = _, % Prevent early binding
    test_solve([X1, +, 2], 4),
    
    X2 = _, 
    test_solve([X2, *, 2], 6),
    
    nl, writeln('4. Nested expression solving:'),
    X3 = _,
    test_solve([[X3, *, 2], +, 3], 7),
    
    X4 = _,
    test_solve([X4, +, [2, *, 3]], 10),
    
    nl, writeln('All tests completed.'),
    halt(0).

test_expr(E) :-
    b(E, eval, R),
    format('Expression: ~w~nEvaluates to: ~w~n', [E, R]).

test_solve(E, Result) :-
    format('Before solving - Expression: ~w = ~w~n', [E, Result]),
    b(E, eval, Result),
    format('After solving - Expression: ~w = ~w~n', [E, Result]).

:- initialization(test).