% :- table p/3.
p(html, element, div).
% Base facts last
p(a, children, p(x, y, z)).
p(a, a, div).
p(b, children, p(x1, y1, z1)).
p(b, a, div).
% p(X, child, [X,S]) :- 
%     p(X, children, p(S, _, _)), !.
% p([X,S], P, O) :- \+

:- dynamic travelled/4.
p(t(X,S), P, O) :- 

    p(html, element, El),
    p(X, a, El),
    \+ travelled(X,El, P, O),
    format(user_error, "X: ~q ~q ~q ~q~n", [X, El, P, O]),
    assertz(travelled(X,El, P, O)),
    p(X, children, p(S, P, O))
    .
f(true).
p(f, is, true) :- f(true). 

% p(A, B, C) :-
%     clause(p(X, children, p(S, P, O)), true),
%     (
%         A = [X,S], B = P, C = O;
%         A = X, B = child, C = [X,S]
%     ).

% b(A, B, C) :-
%     clause(p(X, children, p(S, P, O)), Body),
%     \+ compound(X),
%     call(Body),
%     (
%         (A = [X,S], B = P, C = O);
%         (A = X, B = child, C = [X,S])
%     ).
