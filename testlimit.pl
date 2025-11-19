p(a,b).
p(a,c).
p(a,d).
p(a,e).
p(a,f).

main :-
    % offset(1, p(a,X)),
    forall(
        limit(2, offset(2, p(a,X))),
        % p(a,X),
        format('Result: ~q~n', [X])
    ),
    halt.

:- main.