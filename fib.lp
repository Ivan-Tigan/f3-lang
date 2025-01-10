:- table p/3.

p(a, edge, b).
p(b, edge, c).
p(A, path, B) :- p(A, edge, B).
p(A, path, C) :- p(A, edge, B), p(B, path, C).

p(0, fib, 0).
p(1, fib, 1).
p(N, fib, F) :- 
    N > 1,
    N < 100000,

    N1 is N - 1,
    N2 is N - 2,
    p(N1, fib, F1),
    p(N2, fib, F2),
    F is F1 + F2.