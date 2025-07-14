% test_left_recursion.pl

% Base case first - number rule
expr(num(N), [N|S], S) :- 
    number(N).

% Left-recursive rule - this will infinite loop
expr(plus(Left, Right), S0, S) :-
    expr(Left, S0, S1),    % Calls expr with same input
    S1 = ['+'|S2],
    expr(Right, S2, S).

% Test predicate
test_parse :-
    write('Testing: [1, +, 2]'), nl,
    expr(Result, [1, '+', 2], []),
    write('Result: '), write(Result), nl.

% Main entry point
main :-
    write('Starting left recursion test...'), nl,
    test_parse,
    write('Test completed successfully'), nl.

:- initialization(main).