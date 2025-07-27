:- module(math, []).
:- multifile user:p/3.

% Math builtins for F3
% Provides basic arithmetic operations as predicates

% Addition: A [+ B] Result
user:p(A, [+, B], Result) :-
    number(A),
    number(B),
    Result is A + B.

% Subtraction: A [- B] Result
user:p(A, [-, B], Result) :-
    number(A),
    number(B),
    Result is A - B.

% Multiplication: A [* B] Result
user:p(A, [*, B], Result) :-
    number(A),
    number(B),
    Result is A * B.

% Division: A [/ B] Result
user:p(A, [/, B], Result) :-
    number(A),
    number(B),
    B =\= 0,
    Result is A / B.

% Exponentiation: A [^ B] Result
user:p(A, [^, B], Result) :-
    number(A),
    number(B),
    Result is A ** B.

% Modulo: A [mod B] Result
user:p(A, [mod, B], Result) :-
    number(A),
    number(B),
    B =\= 0,
    Result is A mod B.

% Integer division: A [idiv B] Result
user:p(A, [idiv, B], Result) :-
    number(A),
    number(B),
    B =\= 0,
    Result is A // B.

% Floor division: A [floordiv B] Result
user:p(A, [floordiv, B], Result) :-
    number(A),
    number(B),
    B =\= 0,
    Result is div(A, B).

% Remainder: A [rem B] Result
user:p(A, [rem, B], Result) :-
    number(A),
    number(B),
    B =\= 0,
    Result is A rem B.

% Floor: A floor Result
user:p(A, floor, Result) :-
    number(A),
    Result is floor(A).

% Truncate: A truncate Result
user:p(A, truncate, Result) :-
    number(A),
    Result is truncate(A).

% Round: A round Result
user:p(A, round, Result) :-
    number(A),
    Result is round(A).

user:p(A, sqrt, Result) :-
    number(A),
    Result is sqrt(A).