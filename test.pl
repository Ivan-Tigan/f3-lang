p(bob, a, chef).
p(bob, age, 20).
p(bob, works_at, "Italian Restaurant").
p(alice, a, chef).
p(alice, age, 25).
p(alice, works_at, "French Bistro").
p(carol, a, waiter).
p(carol, age, 22).
p(carol, works_at, "Italian Restaurant").
p(dave, a, waiter).
p(dave, age, 30).
p(dave, works_at, "French Bistro").
p(lengo, hasList, [1, +, [2, *, [3, +, 5]]]).
p(graph([p(X, a, chef), p(X, works_at, "Italian Restaurant")]), =>, graph([p(X, knows, "Italian cuisine"), p(X, skills, "pasta making")])).
p(graph([p(X, a, chef), p(X, works_at, "French Bistro")]), =>, graph([p(X, knows, "French cuisine"), p(X, skills, "wine pairing")])).
p(graph([p(X, a, waiter), p(X, age, Age), p(X, works_at, Place), p(Age, >, 25)]), =>, graph([p(X, position, "senior staff"), p(X, has_responsibility, "training")])).
p(root, a, div).
p(root, class, "flex justify-center").
p(root, id, "root").
p(root, children, [c1, c2, c3]).
p(c1, a, div).
p(c1, class, "flex flex-col justify-center").
p(graph([p([[root, c1, P], graph([p(P, a, chef)])], collect, Ps)]), =>, graph([p(c1, children, Ps)])).
p(graph([p(P, a, chef), p(P, works_at, Place), p([P, " is a chef working at ", Place], sconcat, Text)]), =>, graph([p([root, c1, P], a, div), p([root, c1, P], text, Text)])).
p(X, knows, "Italian cuisine") :-
    p(X, a, chef),
    p(X, works_at, "Italian Restaurant").
p(X, skills, "pasta making") :-
    p(X, a, chef),
    p(X, works_at, "Italian Restaurant").
p(X, knows, "French cuisine") :-
    p(X, a, chef),
    p(X, works_at, "French Bistro").
p(X, skills, "wine pairing") :-
    p(X, a, chef),
    p(X, works_at, "French Bistro").
p(X, position, "senior staff") :-
    p(X, a, waiter),
    p(X, age, Age),
    p(X, works_at, Place),
    b(Age, >, 25).
p(X, has_responsibility, "training") :-
    p(X, a, waiter),
    p(X, age, Age),
    p(X, works_at, Place),
    b(Age, >, 25).
p(c1, children, Ps) :-
    p([[root, c1, P], graph([p(P, a, chef)])], collect, Ps).
p([root, c1, P], a, div) :-
    p(P, a, chef),
    p(P, works_at, Place),
    b([P, " is a chef working at ", Place], sconcat, Text).
p([root, c1, P], text, Text) :-
    p(P, a, chef),
    p(P, works_at, Place),
    b([P, " is a chef working at ", Place], sconcat, Text).

builtin(>).
builtin(<).
builtin(>=).
builtin(<=).
builtin(sconcat).

b(A, >, B) :- A > B.
b(A, <, B) :- A < B.
b(A, >=, B) :- A >= B.
b(A, =<, B) :- A =< B.
% String concatenation builtin
b(XS, sconcat, Res) :- 
    atomic_list_concat(XS, '', Res).
query(Pattern) :-
    findall(Pattern, 
            (p(S,P,O),
             \+ builtin(P),
             \+ (P = '=' ; P = '=>'),  % Skip implications
             \+ P = graph(_),          % Skip graph terms
             Pattern = p(S,P,O)),
            Facts),
    maplist(writeln, Facts).
