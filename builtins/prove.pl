:- module(prove, []).
:- multifile user:p/3.

% prove/3 - executes a goal with optional pagination parameters
% Default: solutions=1, page=0
% Usage: system prove Goal.
% Usage: system (prove solutions 20) Goal.
% Usage: system (prove solutions 20; page 3) Goal.

% Extract parameters from graph, applying defaults
extract_params([], 1, 0).
extract_params([p(prove, solutions, N)|Rest], Solutions, Page) :- !,
    Solutions = N,
    extract_params(Rest, Solutions, Page).
extract_params([p(prove, page, P)|Rest], Solutions, Page) :- !,
    Page = P,
    extract_params(Rest, Solutions, Page).
extract_params([_|Rest], Solutions, Page) :-
    extract_params(Rest, Solutions, Page).

% Main prove predicate - simple goal without parameters
user:p(system, prove, Goal) :-
    limit(1, Goal).

% Main prove predicate - goal with parameters in graph
user:p(system, graph(Params), Goal) :-
    member(p(prove, _, _), Params), !,
    extract_params(Params, Solutions, Page),
    prove_with_pagination(Goal, Solutions, Page).

% Execute goal with pagination using limit/2 and offset/2
prove_with_pagination(Goal, Solutions, Page) :-
    Skip is Page * Solutions,
    offset(Skip, limit(Solutions, Goal)).