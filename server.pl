:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/websocket)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(yall)).

% Make p/3 dynamic right here
% :- dynamic p/3.

% Prevent f3_assert's main from running
:- multifile prolog_load_context/2.
prolog_load_context(argv, _) :- !, fail.

% Use consult instead
:- consult('f3_assert.pl').
:- dynamic p/3.
:- dynamic session_path_socket/3.
:- http_handler('/', handle_all, [prefix]).

:- http_handler('/ws/', handle_ws_request, [prefix,spawn([])]).

session_id_cookie(Request, SessionIdCookie) :-
    memberchk(cookie(Cookies), Request),
    memberchk('connect.sid'=SessionIdAtom, Cookies),
    atom_string(SessionIdAtom, SessionIdCookie), !.

handle_ws_request(Request) :- 
    format(user_error, "Received websocket request: ~w~n~n", [Request]),
    % memberchk(path(PathAtom), Request),
    % atom_string(PathAtom, Path),
    % session_id_cookie(Request, SessionIdCookie)
    http_upgrade_to_websocket(handle_websocket(Request), [], Request)
    % format(user_error, "WS ~q ~n", [X])
    
    .
    
    handle_websocket(Request, WebSocket) :-
        session_id_cookie(Request, SessionIdCookie),
        member(path(PathAtom), Request),
        atom_string(PathAtom, Path),
        asserta(session_path_socket(SessionIdCookie, Path, WebSocket)),
        
        % Send initial content
        (   (p(res, is, [Path, ->, Content]);
            atom_string(TempPathAtom, Path),
            p(res, is, [TempPathAtom, ->, Content]))
        ->  ws_send(WebSocket, text(Content))
        ;   true
        ),
        
        % Start websocket message loop
        ws_loop(Request, WebSocket).
    
    ws_loop(Request, WebSocket) :-
        ws_receive(WebSocket, Message),
        (   Message.opcode = close
        ->  true
        ;   handle_ws_message(Request, Message, WebSocket),
            ws_loop(Request, WebSocket)
        ).
handle_ws_message(Request, Message, WebSocket) :-
    % memberchk(path(P), WebSocket),
    
    format(user_error, "Received websocket message: ~q ~n ~n with message ~n  ~w ~w ~n~n", [ Request, Message, WebSocket]),
    
    % Echo back the message for now
    ws_send(WebSocket, Message).

% Modified handle_portals with better error handling
handle_portals :-
    format(user_error, "Handling portals...~n", []),
    forall((
        session_path_socket(Session, Path, Socket),
        format(user_error, "Found socket: ~w ~w ~w~n", [Session, Path, Socket]),
        (
            p(res, is, [Path, ->, C])
            ; 
            p(res, is, [PathAtom, ->, C]), 
            atom_string(PathAtom, Path)
        )
        ), 
        (
        format(user_error, "Found portal: ~w ~w ~w~n", [Session, Path, C]),
        safe_ws_send(Socket, text(C))
        )).


handle_all(Request) :-
    format(user_error, "Received request: ~w~n", [Request]),
    memberchk(method(Method), Request),
    handle_method(Method, Request).

handle_method(get, Request) :-
    memberchk(path(Path), Request),
    atom_string(Path, PathString),
    format(user_error, "Looking for path: ~w~n~n", [Path]),

    % forall((p(X, Y, Z), \+ Y = '=>') , 
    %        format(user_error, "Found fact: p(~q, ~q, ~q)~n", [X, Y, Z])),
    % forall(p(res, is, [P, ->, C]) , 
    %        format(user_error, "Found page: p(~w, ->, ~w)~n", [P, C])),

    (   (p(res, is, [PathString, ->, Content]);
        atom_string(PathAtom, PathString),
        p(res, is, [PathAtom, ->, Content]))
    ->  format(user_error, "Found content for ~w~n", [Path]),
        format('Content-type: text/html~n~n'),
        write(Content)
    ;   format(user_error, "No content found for ~w~n", [Path]),
        throw(http_reply(not_found(Path)))
    ).

handle_method(post, Request) :-
    memberchk(path(Path), Request),
    http_read_json(Request, json(JSON), []),
    % http_read_data(Request, JSON),
    
    uuid(Guid),
    
    write(user_error, 'POST request:\n'),
    format(user_error, 'Path: ~w\n', [Path]),
    format(user_error, 'JSON: ~w\n', [JSON]),
    format(user_error, 'Guid: ~w\n', [Guid]),
    json_to_triples(JSON, GuidJSON, JSONTriples),
    % Facts to assert
    % JSONGraph = graph(JSONTriples),
    atom_string(Path, PathString),
    BaseFacts = [
        p(Guid, a, inPOST),
        p(Guid, path, PathString)
        % p(Guid, data, JSONGraph)
    ],
    wrap_triples(Guid, data, JSONTriples, WrappedJSONFacts),
    append(BaseFacts, WrappedJSONFacts, Facts),

    format(user_error, "Facts to assert: ~q ~n", [Facts]),
    % b(JSONGraph, query, graph([ p(U, user, Name), p(U, taskName, T)])),
    % format(user_error, "Query facts ~q ~q ~q:~n", [U, Name, T]),
    forall(member(Fact, Facts), asserta(Fact)),
    format(user_error, "Query mutations ~w:~n", [Guid]),
    forall(p(DB, insert, Mutations), 
        (format(user_error, "Insert: ~w~n", [Mutations]),
        insert_db_fact(p(DB, hasGraph, Mutations)))),
    
forall(p(DB, delete, Pattern), 
    (format(user_error, "Delete pattern: ~w~n", [Pattern]),
     forall(p(DB, hasGraph, Actual),
           (subsumes_term(Pattern, Actual) ->
               (format(user_error, "Deleting matching fact: ~w~n", [Actual]),
                delete_db_fact(p(DB, hasGraph, Actual)),
                format(user_error, "~n Deletiosn done ~n", [])
            )
           ; true)))),

    
    % format(user_error, "Checking asserted facts for Guid ~w:~n", [Guid]),
    forall(p(Guid, Pred, Obj), 
           format(user_error, "Found: p(~w, ~w, ~w)~n", [Guid, Pred, Obj])),
            retract(p(Guid, Pred, Obj)),
    % retractall(p(Guid, X, Y)),
    
    % format(user_error, "Retracted all facts for Guid ~w~n", [Guid]),
    
    
    % % Verify they're gone
    % format(user_error, "Verifying retraction - remaining facts for Guid ~w:~n", [Guid]),
    % forall(p(G, data, Obj), 
    %        format(user_error, "Still found: ~q ~n", [p(G, data, Obj)])),
    
    % format(user_error, "Current DB ~w:~n", [DB]),
    % forall(p(DB, hasGraph, Obj), 
    %        format(user_error, "DBFact: ~q ~n", [p(DB, hasGraph, Obj)])),
    handle_portals,
    reply_json_dict(_{status: success, guid: Guid}).

uuid(Guid) :-
    get_time(Now),
    format(atom(Id), 'post_~w', [Now]),
    b(Id, sha256, G),
    sub_string(G, 0, 6, _, Guid).
:- initialization(main, main).

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

% Handle string values - convert to string type
pair_to_triples(ParentGuid, Key-Value, [p(ParentGuid, Key, String)]) :-
    atom(Value),
    !,
    atom_string(Value, String).

% Handle numeric values - keep as is
pair_to_triples(ParentGuid, Key-Value, [p(ParentGuid, Key, Value)]) :-
    number(Value),
    !.

% Handle other simple values
pair_to_triples(ParentGuid, Key-Value, [p(ParentGuid, Key, Value)]).
% Handle nested objects
wrap_triples(_,_, [], []).
wrap_triples(S, P, [H|T], [p(S, P, H)|Rest]) :-
    wrap_triples(S, P, T, Rest).

assertz_once(Fact) :-
    \+ Fact,    % Only proceed if Fact is not already true
    !,          % Cut to prevent backtracking
    assertz(Fact).
assertz_once(_).  % If fact exists, succeed silently
    
    % Add error handling to WebSocket sends
safe_ws_send(Socket, Message) :-
    catch(
        ws_send(Socket, Message),
        Error,
        (
            format(user_error, "WebSocket send failed: ~w~n", [Error]),
            retractall(session_path_socket(_, _, Socket))
        )
    ).

main([F3File, Port]) :-
    atom_number(Port, PortNum),
    process_input(F3File),
    http_server(http_dispatch, [port(PortNum)]),
    format('Server started on port ~w~n', [PortNum]),
    thread_get_message(_).