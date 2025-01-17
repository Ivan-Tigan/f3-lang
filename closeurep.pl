p(a, edge, b).
p(b,edge,c).
p(c,edge, d).

p(X,closure, Y) :- p(X, edge, Y).
p(X,closure, Z) :- p(X,closure, Y), p(Y,edge, Z).