p(A, is, X) :- X = 2.
p(A, is, 1).
main :-
    p("bob", is, X),
    write(X), nl
    .