:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).

% Make p/3 dynamic right here
% :- dynamic p/3.

% Prevent f3_assert's main from running
:- multifile prolog_load_context/2.
prolog_load_context(argv, _) :- !, fail.

% Use consult instead
:- consult('f3_assert.pl').
:- dynamic p/3.

:- http_handler('/', handle_all, [prefix]).

handle_all(Request) :-
    memberchk(method(Method), Request),
    handle_method(Method, Request).

handle_method(get, Request) :-
    memberchk(path(Path), Request),
    atom_string(Path, PathString),
    format(user_error, "Looking for path: ~w~n", [Path]),
    
    (   (p(res, is, [PathString, ->, Content]);
        atom_string(PathAtom, PathString),
        p(res, is, [PathAtom, ->, Content]))
    ->  format(user_error, "Found content for ~w~n", [Path]),
        format('Content-type: text/html~n~n'),
        write(Content)
    ;   format(user_error, "No content found for ~w~n", [Path]),
        throw(http_reply(not_found(Path)))
    ).
json_to_triples(Dict, Guid, Triples) :-
    % Generate a UUID for this object
    uuid(Guid),
    % Convert each key-value pair to triples
    dict_pairs(Dict, _Type, Pairs),
    maplist(pair_to_triples(Guid), Pairs, TripleLists),
    append(TripleLists, Triples).

% Handle nested objects
pair_to_triples(ParentGuid, Key-Value, [p(ParentGuid, Key, ChildGuid)|ChildTriples]) :-
    is_dict(Value),
    !,
    json_to_triples(Value, ChildGuid, ChildTriples).

% Handle arrays
pair_to_triples(ParentGuid, Key-Value, [p(ParentGuid, Key, Value)]) :-
    is_list(Value),
    !.

% Handle simple values
pair_to_triples(ParentGuid, Key-Value, [p(ParentGuid, Key, Value)]).

handle_method(post, Request) :-
    memberchk(path(Path), Request),
    http_read_json(Request, json(JSON), []),
    
    uuid(Guid),
    
    write(user_error, 'POST request:\n'),
    format(user_error, 'Path: ~w\n', [Path]),
    format(user_error, 'JSON: ~w\n', [JSON]),
    format(user_error, 'Guid: ~w\n', [Guid]),
    json_to_triples(JSON, GuidJSON, JSONTriples),
    % Facts to assert
    Facts = [
        p(Guid, a, inPOST),
        p(Guid, path, Path),
        p(Guid, data, graph(JSONTriples))
    ],
    
    forall(member(Fact, Facts), assertz(Fact)),
    format(user_error, "Query mutations ~w:~n", [Guid]),
    forall(p(DB, mutations, Mutations), format(user_error, "Mutations: ~w~n", [Mutations])),

    format(user_error, "Checking asserted facts for Guid ~w:~n", [Guid]),
    forall(p(Guid, Pred, Obj), 
           format(user_error, "Found: p(~w, ~w, ~w)~n", [Guid, Pred, Obj])),
            retractall(p(Guid, _, _)),
    format(user_error, "Retracted all facts for Guid ~w~n", [Guid]),
    
    
    % Verify they're gone
    format(user_error, "Verifying retraction - remaining facts for Guid ~w:~n", [Guid]),
    forall(p(Guid, Pred, Obj), 
           format(user_error, "Still found: p(~w, ~w, ~w)~n", [Guid, Pred, Obj])),

    reply_json_dict(_{status: success, guid: Guid}).

uuid(Id) :-
    get_time(Now),
    format(atom(Id), 'post_~w', [Now]).

:- initialization(main, main).

main([F3File, Port]) :-
    atom_number(Port, PortNum),
    process_input(F3File),
    http_server(http_dispatch, [port(PortNum)]),
    format('Server started on port ~w~n', [PortNum]),
    thread_get_message(_).