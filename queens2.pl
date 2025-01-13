% render solutions nicely.

%%	n_queens(?N, ?Cols) is nondet.
%
%	@param The k-th element of Cols is the column number of the
%	queen in row k.
%	@author Markus Triska

:- use_module(library(clpfd)).

n_queens(N, Qs) :-
	length(Qs, N),
	Qs ins 1..N,
	safe_queens(Qs).

safe_queens([]).
safe_queens([Q|Qs]) :-
	safe_queens(Qs, Q, 1),
	safe_queens(Qs).

safe_queens([], _, _).
safe_queens([Q|Qs], Q0, D0) :-
	Q0 #\= Q,
	abs(Q0 - Q) #\= D0,
	D1 #= D0 + 1,
	safe_queens(Qs, Q0, D1).


/** <examples>

?- n_queens(8, Qs), labeling([ff], Qs).
?- n_queens(24, Qs), labeling([ff], Qs).
?- n_queens(100, Qs), labeling([ff], Qs).

*/

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