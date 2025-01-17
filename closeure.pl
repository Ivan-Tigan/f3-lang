edge(a,b).
edge(b,c).
edge(c,d).

closure(X, Y) :- edge(X, Y).
closure(X, Z) :- closure(Y, Z), edge(X, Y).


% closure(X, Y) :- edge(X, Y).
% closure(X, Z) :- edge(X, Y), closure(Y, Z).