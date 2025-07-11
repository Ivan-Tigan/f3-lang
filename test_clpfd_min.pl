:- use_module(library(clpfd)).

% Test 1: Working case equivalent
test_working :-
    writeln('=== Working case: 3 + 7*Y = X + 10 ==='),
    X in 20..60,
    Y #> 5,
    3 + 7*Y #= X + 10,
    findall([Y,X], label([Y,X]), Solutions),
    format('Solutions: ~w~n', [Solutions]).

% Test 2: Non-working case equivalent  
test_nonworking :-
    writeln('=== Non-working case: 3 + 7*Y = min(X+10, X+20) ==='),
    X in 20..60,
    Y #> 5,
    3 + 7*Y #= min(X + 10, X + 20),
    findall([Y,X], label([Y,X]), Solutions),
    format('Solutions: ~w~n', [Solutions]).

% Test 3: Simplified min test
test_min_simple :-
    writeln('=== Simple min test: Z = min(X+10, X+20) ==='),
    X in 20..30,
    Z #= min(X + 10, X + 20),
    findall([X,Z], label([X,Z]), Solutions),
    format('Solutions: ~w~n', [Solutions]).

:- test_working.
:- test_nonworking.
:- test_min_simple.