:- use_module(library(modeling)).

queens(N, Queens):-
  int_array(Queens, [N], 1..N),
  for_all([I in 1..N-1, D in 1..N-I],
          (Queens[I] #\= Queens[I+D],
           Queens[I] #\= Queens[I+D]+D,
           Queens[I] #\= Queens[I+D]-D)),
  satisfy(Queens).

show(Queens):-
    array(Queens, [N]),
    for_all([I, J] in 1..N,
            let([Q = if(Queens[J] = I, 'Q', '.'),
                 B = if(J = N, '\n', ' ')],
                format("~w~w",[Q,B]))).
