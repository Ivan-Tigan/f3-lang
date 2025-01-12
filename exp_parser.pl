:- use_module(library(dcg/basics)).

% Top level rule for expression
expr(E) --> term(T), expr_rest(T, E).

% Handle + and - at lowest precedence
expr_rest(Left, E) --> 
    ['+'], term(Right), expr_rest([Left, +, Right], E).
expr_rest(Left, E) --> 
    ['-'], term(Right), expr_rest([Left, -, Right], E).
expr_rest(E, E) --> [].

% Handle * and / at higher precedence  
term(T) --> factor(F), term_rest(F, T).

term_rest(Left, T) --> 
    ['*'], factor(Right), term_rest([Left, *, Right], T).
term_rest(Left, T) --> 
    ['/'], factor(Right), term_rest([Left, /, Right], T).
term_rest(T, T) --> [].

% Base cases - numbers and nested expressions
factor(N) --> [N], { number(N) }.
factor(E) --> [List], { is_list(List) }, { phrase(expr(E), List) }.

% Test predicate with pretty printing
test(Input, Result) :-
    phrase(expr(Result), Input),
    write('Input:  '), writeq(Input), nl,
    write('Result: '), writeq(Result), nl.

% Example tests
:- begin_tests(expr_parser).

test(nested_deep) :-
    test([1, +, 3, *, [5, -, [2,+,1]]], _).

test(complex_nested) :-
    test([1, +, [2, *, 3], *, [4, -, 5]], _).

:- end_tests(expr_parser).