% fib.pl
% Base cases
:- table fib/2.
fib(0, 0).
fib(1, 1).

% Recursive rule
fib(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.

% Store result in a fact
:- fib(200, X), assertz(result(X)).