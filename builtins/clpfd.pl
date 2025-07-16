:- module(f3_clpfd, []).
:- use_module(library(clpfd)).
:- multifile user:p/3.

% F3 CLP(FD) - Complete rewrite with rigorous list-based parser
% Supports all CLP(FD) expressions and constraints with proper function syntax

% ============================================================================
% TOP-LEVEL CONSTRAINT PARSER
% ============================================================================

% Main constraint parsing entry point
% Parses: [constraint & constraint & ...]
parse_constraints(ConstraintList, Constraints) :-
    phrase(constraints(Constraints), ConstraintList).

% Constraint grammar: handles multiple constraints with & separator
constraints(Constraints) -->
    constraint_expr(First),
    constraint_rest(First, Constraints).

constraint_rest(First, (First, Rest)) -->
    [&],
    constraint_expr(Second),
    constraint_rest(Second, Rest).
constraint_rest(Constraint, Constraint) --> [].

% ============================================================================
% CONSTRAINT EXPRESSION PARSER
% ============================================================================

% Parse individual constraint expressions
constraint_expr(Constraint) -->
    expression(Left),
    comparison_op(Op),
    expression(Right),
    { constraint_op_to_clpfd(Op, Left, Right, Constraint) }.

% Domain constraints: ?X in Low High
constraint_expr(Constraint) -->
    expression(Var),
    [in],
    expression(Low),
    expression(High),
    { Constraint = (Var in Low..High) }.

% Domain constraints: ?X in Domain
constraint_expr(Constraint) -->
    expression(Var),
    [in],
    domain_expr(Domain),
    { Constraint = (Var in Domain) }.

% Multiple variable domains: ?X ?Y ?Z ins Low High
constraint_expr(Constraint) -->
    var_list(Vars),
    [ins],
    expression(Low),
    expression(High),
    { Constraint = (Vars ins Low..High) }.

% Multiple variable domains: ?X ?Y ?Z ins Domain
constraint_expr(Constraint) -->
    var_list(Vars),
    [ins],
    domain_expr(Domain),
    { Constraint = (Vars ins Domain) }.

% Global constraints: all_distinct([?X, ?Y, ?Z])
constraint_expr(Constraint) -->
    [X],
    {\+ var(X), X = all_distinct},
    [List],
    { is_list(List), Constraint = all_distinct(List) }.

% Global constraints: global_cardinality([?X, ?Y, ?Z], [1-2, 3-1])
constraint_expr(Constraint) -->
    [X],
    {\+ var(X), X = global_cardinality},
    [Vars],
    [Pairs],
    { is_list(Vars), is_list(Pairs), Constraint = global_cardinality(Vars, Pairs) }.

% Sum constraint: sum([?X, ?Y, ?Z], #=, ?Total)
constraint_expr(Constraint) -->
    [X],
    {\+ var(X), X = sum},
    [Vars],
    comparison_op(Op),
    expression(Expr),
    { is_list(Vars), constraint_op_to_clpfd_sum(Op, Vars, Expr, Constraint) }.

% Element constraint: element(?N, [10, 20, 30], ?Value)
constraint_expr(Constraint) -->
    [X],
    {\+ var(X), X = element},
    expression(Index),
    [List],
    expression(Value),
    { is_list(List), Constraint = element(Index, List, Value) }.

% ============================================================================
% EXPRESSION PARSER - HANDLES PRECEDENCE AND FUNCTIONS
% ============================================================================

% Main expression parser with proper precedence
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
    { binary_op_to_clpfd(Op, Left, Right, Temp) },
    mul_expr(Temp, Result).
mul_expr(Left, Result) -->
    [^],
    factor(Right),
    { Result = (Left ^ Right) },
    mul_expr(Result, _).
mul_expr(Result, Result) --> [].


% ============================================================================
% FACTOR PARSER - HANDLES FUNCTIONS, GROUPING, AND ATOMS
% ============================================================================

% Function expressions: [min ?X ?Y], [max ?X ?Y], [abs ?X], etc.
factor(Result) -->
    [List],
    { 
        is_list(List),
        List = [FuncName|Args],
        nonvar(FuncName),
        function_name(FuncName)
    },
    { parse_function_expr(FuncName, Args, Result) }.

% Explicit grouping: [?X + ?Y] for precedence
factor(Result) -->
    [List],
    { 
        is_list(List),
        \+ (List = [First|_], nonvar(First), function_name(First))
    },
    { phrase(expression(Result), List) }.

% Unary minus: [-?X] becomes -(?X)
factor(Result) -->
    [List],
    { 
        is_list(List),
        List = [-, Operand]
    },
    { phrase(expression(Expr), [Operand]), Result = (-Expr) }.

% Basic atoms: variables, numbers, atoms
factor(X) -->
    [X],
    { 
        \+ is_list(X),
        (   var(X) -> true
        ;   number(X) -> true  
        ;   atom(X), \+ reserved_word(X)
        )
    }.

% ============================================================================
% FUNCTION EXPRESSION PARSER
% ============================================================================

% Parse function expressions like [min ?X ?Y], [max ?X ?Y ?Z], [abs ?X]
parse_function_expr(min, Args, Result) :-
    parse_function_args(Args, ParsedArgs),
    build_min_max(min, ParsedArgs, Result).

parse_function_expr(max, Args, Result) :-
    parse_function_args(Args, ParsedArgs),
    build_min_max(max, ParsedArgs, Result).

parse_function_expr(abs, [Arg], abs(ParsedArg)) :-
    parse_single_arg(Arg, ParsedArg).

% Modulo operations
parse_function_expr(mod, [Left, Right], Result) :-
    parse_single_arg(Left, LeftExpr),
    parse_single_arg(Right, RightExpr),
    Result = (LeftExpr mod RightExpr).

parse_function_expr(rem, [Left, Right], Result) :-
    parse_single_arg(Left, LeftExpr),
    parse_single_arg(Right, RightExpr),
    Result = (LeftExpr rem RightExpr).

% Division operations
parse_function_expr(div, [Left, Right], Result) :-
    parse_single_arg(Left, LeftExpr),
    parse_single_arg(Right, RightExpr),
    Result = (LeftExpr div RightExpr).

% Bitwise operations
parse_function_expr('\\', [Arg], Result) :-
    parse_single_arg(Arg, ParsedArg),
    Result = (\(ParsedArg)).

parse_function_expr('/\\', [Left, Right], Result) :-
    parse_single_arg(Left, LeftExpr),
    parse_single_arg(Right, RightExpr),
    Result = (LeftExpr /\ RightExpr).

parse_function_expr('\\/', [Left, Right], Result) :-
    parse_single_arg(Left, LeftExpr),
    parse_single_arg(Right, RightExpr),
    Result = (LeftExpr \/ RightExpr).

parse_function_expr(xor, [Left, Right], Result) :-
    parse_single_arg(Left, LeftExpr),
    parse_single_arg(Right, RightExpr),
    Result = (LeftExpr xor RightExpr).

parse_function_expr('>>', [Left, Right], Result) :-
    parse_single_arg(Left, LeftExpr),
    parse_single_arg(Right, RightExpr),
    Result = (LeftExpr >> RightExpr).

parse_function_expr('<<', [Left, Right], Result) :-
    parse_single_arg(Left, LeftExpr),
    parse_single_arg(Right, RightExpr),
    Result = (LeftExpr << RightExpr).

% Bit manipulation
parse_function_expr(msb, [Arg], msb(ParsedArg)) :-
    parse_single_arg(Arg, ParsedArg).

parse_function_expr(lsb, [Arg], lsb(ParsedArg)) :-
    parse_single_arg(Arg, ParsedArg).

parse_function_expr(popcount, [Arg], popcount(ParsedArg)) :-
    parse_single_arg(Arg, ParsedArg).

% Helper: parse function arguments
parse_function_args([], []).
parse_function_args([Arg|Args], [ParsedArg|ParsedArgs]) :-
    parse_single_arg(Arg, ParsedArg),
    parse_function_args(Args, ParsedArgs).

% Helper: parse a single argument (can be expression or simple value)
parse_single_arg(Arg, ParsedArg) :-
    (   is_list(Arg) ->
        phrase(expression(ParsedArg), Arg)
    ;   ParsedArg = Arg
    ).

% Helper: build min/max expressions recursively
build_min_max(_, [], _) :- fail.
build_min_max(_, [Single], Single).
build_min_max(Op, [First, Second], Result) :-
    (Op = min -> Result = min(First, Second) ; Result = max(First, Second)).
build_min_max(Op, [First|Rest], Result) :-
    build_min_max(Op, Rest, RestResult),
    (Op = min -> Result = min(First, RestResult) ; Result = max(First, RestResult)).

% ============================================================================
% DOMAIN EXPRESSION PARSER
% ============================================================================

% Parse domain expressions: 1..10, 1\/3..5\/8..10
domain_expr(Domain) -->
    domain_term(First),
    domain_rest(First, Domain).

domain_rest(First, First \/ Rest) -->
    [\\/],
    domain_term(Second),
    domain_rest(Second, Rest).
domain_rest(Domain, Domain) --> [].

domain_term(Low..High) -->
    [Low], [..], [High],
    { number(Low), number(High) }.

domain_term(Value) -->
    [Value],
    { number(Value) }.

% ============================================================================
% VARIABLE LIST PARSER
% ============================================================================

% Parse variable lists: [?X ?Y ?Z]
var_list([Var]) -->
    [Var], { var(Var) }.
var_list([Var|Rest]) -->
    [Var], { var(Var) },
    var_list(Rest).

% ============================================================================
% OPERATOR DEFINITIONS
% ============================================================================

% Arithmetic operators
add_op(+) --> [+].
add_op(-) --> [-].

mul_op(*) --> [*].
mul_op(/) --> [/].

% Comparison operators
comparison_op(=) --> [=].
comparison_op(<) --> [<].
comparison_op(>) --> [>].
comparison_op(=<) --> [=<].
comparison_op(>=) --> [>=].
comparison_op(neq) --> [neq].

% Function names that can appear at start of lists
function_name(min).
function_name(max).
function_name(abs).
function_name(mod).
function_name(rem).
function_name(div).
function_name('\\').
function_name('/\\').
function_name('\\/').
function_name(xor).
function_name('>>').
function_name('<<').
function_name(msb).
function_name(lsb).
function_name(popcount).

% Reserved words that cannot be treated as variables
reserved_word(X) :-
    member(X, [+, -, *, /, =, <, >, =<, >=, neq, in, ins, &, all_distinct, 
               global_cardinality, sum, element, min, max, abs, mod, rem, div]).

% ============================================================================
% CONSTRAINT MAPPING TO CLP(FD)
% ============================================================================

% Map comparison operators to CLP(FD) constraints
constraint_op_to_clpfd(=, Left, Right, (Left #= Right)).
constraint_op_to_clpfd(<, Left, Right, (Left #< Right)).
constraint_op_to_clpfd(>, Left, Right, (Left #> Right)).
constraint_op_to_clpfd(=<, Left, Right, (Left #=< Right)).
constraint_op_to_clpfd(>=, Left, Right, (Left #>= Right)).
constraint_op_to_clpfd(neq, Left, Right, (Left #\= Right)).

% Map sum constraints
constraint_op_to_clpfd_sum(=, Vars, Expr, sum(Vars, #=, Expr)).
constraint_op_to_clpfd_sum(<, Vars, Expr, sum(Vars, #<, Expr)).
constraint_op_to_clpfd_sum(>, Vars, Expr, sum(Vars, #>, Expr)).
constraint_op_to_clpfd_sum(=<, Vars, Expr, sum(Vars, #=<, Expr)).
constraint_op_to_clpfd_sum(>=, Vars, Expr, sum(Vars, #>=, Expr)).
constraint_op_to_clpfd_sum(neq, Vars, Expr, sum(Vars, #\=, Expr)).

% Map binary operators
binary_op_to_clpfd(+, L, R, L + R).
binary_op_to_clpfd(-, L, R, L - R).
binary_op_to_clpfd(*, L, R, L * R).
binary_op_to_clpfd(/, L, R, L // R).  % Integer division for CLP(FD)

% ============================================================================
% F3 INTERFACE PREDICATES
% ============================================================================

% Main constraint predicate: clpfd constraint [constraints]
user:p(clpfd, constraint, ConstraintArray) :-
    parse_constraints(ConstraintArray, Constraints),
    call(Constraints).

% Simple labeling: clpfd label [variables]
user:p(clpfd, label, Variables) :-
    label(Variables).

% Labeling with all solutions: clpfd labels [[variables] solutions]
user:p(clpfd, labels, [Variables, AllSolutions]) :-
    findall(Variables, label(Variables), AllSolutions), !.

