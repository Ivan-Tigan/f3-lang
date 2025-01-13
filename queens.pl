:- use_module(library(clpfd)).

n_queens(N, Queens) :-
    length(Queens, N),
    Queens ins 1..N,
    safe_queens(Queens).

safe_queens([]).
safe_queens([Q|Queens]) :-
    safe_queens(Queens, Q, 1),
    safe_queens(Queens).

safe_queens([], _, _).
safe_queens([Q|Queens], Q0, D0) :-
    Q0 #\= Q,
    abs(Q0 - Q) #\= D0,
    D1 #= D0 + 1,
    safe_queens(Queens, Q0, D1).

run_queens(N) :-
    get_time(Start),
    n_queens(N, Queens),
    label(Queens),
    get_time(End),
    Time is End - Start,
    format('Solution: ~w~n', [Queens]),
    format('Time: ~3f seconds~n', [Time]),
    halt.

% Run with N=8
:- run_queens(20).