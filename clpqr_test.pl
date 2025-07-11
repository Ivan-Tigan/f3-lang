:- use_module(library(clpq)).

main :- 
  {
    X1=<50,
    X2=<200,
    X1+0.2*X2=<72,
    150*X1+25*X2=<10000,
    Z = 250*X1+45*X2,
    2*(Y1+Y2)=X1,
    Y2=10
  },
  maximize(Z),
  format('X1: ~w, X2: ~w, Z: ~w, Y1: ~w, Y2: ~w~n', [X1, X2, Z, Y1, Y2]).
  