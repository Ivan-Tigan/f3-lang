:- use_module(module1).
:- use_module(module2).
:- multifile user:likes/2.

likes(mary, sushi).
likes(john, pizza).

test_likes :-
    findall([Person, Food], likes(Person, Food), Results),
    writeln(Results).
