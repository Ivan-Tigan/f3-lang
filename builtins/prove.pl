:- module(prove, []).
:- multifile user:p/3.

% prove/3 - executes a goal with optional pagination parameters
% Default: solutions=1, page=0
% Usage: system prove Goal.
% Usage: system (prove solutions 20) Goal.
% Usage: system (prove solutions 20; page 3) Goal.

% Convert list of facts to conjunction
l2c([], true).
l2c([p(system, P, [])|Xs], (!, Y)) :- \+ var(P), P = cut, !, l2c(Xs, Y).
l2c([X|Xs], (X, Y)) :- l2c(Xs, Y).

% Extract parameters from graph, applying defaults
extract_params([], 1, 0).
extract_params(Params, Solutions, Page) :- 
    member(p(prove, solutions, Solutions), Params),
    member(p(prove, page, Page), Params), !.
extract_params(Params, Solutions, 0) :- 
    member(p(prove, solutions, Solutions), Params), !.
extract_params(Params, 1, Page) :- 
    member(p(prove, page, Page), Params), !.



% Main prove predicate - simple goal with graph
user:p(system, prove, graph(Goal)) :-
    l2c(Goal, Conj),
    limit(1, Conj).

% Main prove predicate - goal with parameters in graph
user:p(system, graph(Params), graph(Goal)) :-
    member(p(prove, _, _), Params), !,
    extract_params(Params, Solutions, Page),
    l2c(Goal, Conj),
    Skip is Page * Solutions,
    % user:p(system, log, [proving, with, Solutions, Page, Skip]),
    limit(Solutions, (offset(Skip, Conj), 
        true
        % format(user_error, 'Offset prove ~q~n', [Conj])
    
    )).
