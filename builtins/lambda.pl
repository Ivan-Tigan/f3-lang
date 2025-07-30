:- module(lambda, []).
:- multifile user:p/3.

l2c([], true).
l2c([p(system, P, [])|Xs], (!, Y)) :- \+ var(P), P = cut, !, l2c(Xs, Y).
l2c([X|Xs], (X, Y)) :- l2c(Xs, Y).

user:p(Input, [Input, graph(Facts), Output], Output) :- 
    l2c(Facts, Conj), call(Conj).   