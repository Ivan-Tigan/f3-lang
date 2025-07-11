:- consult('f3_assert.pl').

test_equivalent :-
    writeln('=== Testing equivalent program ==='),
    
    % The F3 program translates to:
    % [3 +  [7 * ?Y]] = [min [?X + 10] [?X + 20]]
    % ?X in [20 60]
    % ?Y > 5
    % [?Y ?X] labels ?AllSolutions
    
    writeln('Testing F3 equivalent:'),
    user:p([3, +, [7, *, Y]], =, [min, [X, +, 10], [X, +, 20]]),
    user:p(X, in, [20, 60]),
    user:p(Y, >, 5),
    user:p([Y, X], labels, AllSolutions),
    format('AllSolutions = ~w~n', [AllSolutions]).

:- test_equivalent.