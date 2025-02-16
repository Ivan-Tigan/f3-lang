:- module(cache, [
    write_cache/3,
    access_cache/3
]).

:- dynamic cache/4.  % cache(Args, Operation, Result, LRUTimestamp)

write_cache(Args, Operation, Result) :-
    get_time(Now),
    assertz(cache(Args, Operation, Result, Now)).

access_cache(Args, Operation, Result) :-
    % Cleanup old entries first
    get_time(Now),
    Threshold is Now - 60,  % 1 minute
    forall(
        (cache(_, _, _, LRU), LRU < Threshold),
        retract(cache(_, _, _, LRU))
    ),
    % Try to get from cache and update LRU time
    cache(Args, Operation, Result, _),
    !,  % Cut to prevent backtracking if found
    % Update LRU timestamp
    get_time(Now),
    retractall(cache(Args, Operation, Result, _)),
    assertz(cache(Args, Operation, Result, Now)).

% Helper predicate to clear entire cache if needed
clear_cache :-
    retractall(cache(_,_,_,_)).

% b(A, [cache, P], B) :- access_cache(A, [cache, P], B), !.
% b(A, [cache, P], B) :- p(A, P, B), !, write_cache(A, [cache, P], B).
% b(A, [cache, P], B) :- b(A, P, C), !, write_cache(A, [cache, P], C).