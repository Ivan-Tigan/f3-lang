:- module(lambda, []).
:- multifile user:p/3.

l2c([], true).
l2c([p(system, P, [])|Xs], (!, Y)) :- \+ var(P), P = cut, !, l2c(Xs, Y).
l2c([X|Xs], (X, Y)) :- l2c(Xs, Y).
user:p(system, call, graph(Facts)) :- 
    l2c(Facts, Conj), call(Conj).

user:p(Witness, distinct, graph(Facts)) :- 
    l2c(Facts, Conj), distinct(Witness, Conj).

user:p(Input, [fun, Input, ->, graph(Facts), Output], return, Output) :- 
    l2c(Facts, Conj), call(Conj).   

user:p(G, =>, graph([p(X, Pred, Y)])) :- 
    user:p(X, chain(Pred,G), Y).

user(graph([p(X, Pred2, Y)]), =>, graph([p(X, Pred1, Y)])) :- 
    user:p(Pred1, <-, Pred2).

user:p(XS, [map, Pred], Res) :- 
    user:p([R, graph([p(XS, iter, X), p(X, Pred, R)])], collect, Res).

user:p(G, [->, Key], Res) :-
    user:p(G, match, graph([p(Key, =, Res)])).