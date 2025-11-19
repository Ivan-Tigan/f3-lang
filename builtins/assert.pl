:- module(assert_builtins, []).

:- multifile user:p/3.

% Assert a triple dynamically into the Prolog database
% Usage: system assert (subject predicate object.)
user:p(system, assert, p(S, P, O)) :-
    assertz(user:p(S, P, O)),
    format(user_error, "Asserted: p(~q, ~q, ~q)~n", [S, P, O]).

% Retract a single matching triple from the database
% Usage: system retract (subject predicate object.)
user:p(system, retract, p(S, P, O)) :-
    retract(user:p(S, P, O)),
    format(user_error, "Retracted: p(~q, ~q, ~q)~n", [S, P, O]).

% Retract all matching triples from the database
% Usage: system retractAll (subject predicate object.)
user:p(system, retractAll, p(S, P, O)) :-
    findall(p(S, P, O), user:p(S, P, O), Matches),
    length(Matches, Count),
    retractall(user:p(S, P, O)),
    format(user_error, "Retracted ~w matching triple(s)~n", [Count]).