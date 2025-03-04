:- module(f3_websocket, []).

:- use_module(library(http/websocket)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

:- multifile user:b/3.
:- multifile user:builtin/1.

% Register wsSend builtin
user:builtin(wsSend).

% Store active WebSocket connections
:- dynamic ws_connection/4.  % ws_connection(ID, Path, Headers, WebSocket)

% This predicate checks if a request is a WebSocket upgrade request
is_websocket_request(Request) :-
    memberchk(connection(Connection), Request),
    (   Connection == 'Upgrade'
    ;   member('Upgrade', Connection)
    ),
    memberchk(upgrade(websocket), Request),
    memberchk(sec_websocket_version(_), Request),
    !.

% Hook into server.pl's request handling - this will be called by server.pl
% WebSocket request handler - simplified to use standard Prolog inference
handle_websocket_upgrade(Request, _) :-
    % Extract path and protocol
    memberchk(path(PathAtom), Request),
    atom_string(PathAtom, Path),
    
    % Extract protocol 
    (memberchk(sec_websocket_protocol(ProtocolAtom), Request) -> 
        atom_string(ProtocolAtom, Protocol) 
    ; 
        Protocol = ""
    ),
    
    % Generate connection ID
    generate_ws_id(ConnectionId),
    
    % Create WebSocket connection pattern that matches the F3 rule structure
    % This matches the structure: (WS match (...))
    ConnectionPattern = graph([
            p(ConnectionId, a, wsConnection),
            p(ConnectionId, path, Path),
            p(ConnectionId, headers, graph([
                p("Sec-WebSocket-Protocol", =, Protocol)
            ]))
    ]),
    
    format(user_error, "Trying to match WebSocket pattern: ~q~n", [ConnectionPattern]),
    
    % Find matching rule using standard Prolog inference
    (   p(ConnectionPattern, response, graph(Response))
    ->  format(user_error, "Found matching WebSocket rule~n", [])
    ;   format(user_error, "No matching WebSocket rule found - rejecting~n", []),
        throw(http_reply(forbidden("No matching handler found"), [], []))
    ),
    
    format(user_error, "WebSocket response: ~q~n", [Response]),
    
    % Check if the response is accepting the connection
    (   memberchk(p(_, status, "accept"), Response)
    ->  % Accept connection
        % Extract response headers for upgrading
        (   memberchk(p(_, headers, graph(ResponseHeaderTriples)), Response)
        ->  findall(Name=Value,
                   member(p(Name, =, Value), ResponseHeaderTriples),
                   ResponseHeaders)
        ;   ResponseHeaders = []
        ),
        
        % Extract protocol from response headers if present
        (   memberchk(p("Sec-WebSocket-Protocol", =, ResponseProtocolString), ResponseHeaderTriples)
        ->  atom_string(ResponseProtocol, ResponseProtocolString), Protocols = [ResponseProtocol]
        ;   Protocol \= "" -> Protocols = [Protocol] ; Protocols = []
        ),
        
        % Store headers for later matching
        Headers = ["Sec-WebSocket-Protocol"=Protocol],
        
        % Upgrade to WebSocket with proper protocol
        http_upgrade_to_websocket(ws_handler(ConnectionId, Headers, Path),
                                 [subprotocols(Protocols),
                                  headers(ResponseHeaders)],
                                 Request),
        format(user_error, "WebSocket connection upgrade initiated: ~q~n", [ConnectionId])
    ;   % Reject with reason from rule
        (   memberchk(p(_, reason, Reason), Response)
        ->  RejectionReason = Reason
        ;   RejectionReason = "Connection rejected by rule"
        ),
        format(user_error, "Rejecting WebSocket connection: ~q~n", [RejectionReason]),
        throw(http_reply(forbidden(RejectionReason), [], []))
    ).

% WebSocket handler after successful upgrade
ws_handler(ConnectionId, HeaderPairs, Path, WebSocket) :-
    format(user_error, "WebSocket connection established: ~w~n", [ConnectionId]),
    
    % Store the connection
    assertz(ws_connection(ConnectionId, Path, HeaderPairs, WebSocket)),
    
    % Start receive loop
    catch(
        ws_receive_loop(ConnectionId, WebSocket),
        Error,
        (format(user_error, "WebSocket error: ~w~n", [Error]),
         cleanup_connection(ConnectionId))
    ).

% Generate WebSocket connection ID
generate_ws_id(ID) :-
    get_time(Now),
    format(atom(ID), 'ws_~w', [Now]).

% Cleanup connection
cleanup_connection(ConnectionId) :-
    retractall(ws_connection(ConnectionId, _, _, _)),
    format(user_error, "WebSocket connection closed: ~w~n", [ConnectionId]).

% WebSocket message receive loop
ws_receive_loop(ConnectionId, WebSocket) :-
    ws_receive(WebSocket, Message, [format(json)]),
    format(user_error, "Received WebSocket message: ~w~n", [Message]),
    
    % Handle message based on type
    (   Message.opcode == close
    ->  % Connection closed
        cleanup_connection(ConnectionId)
    ;   % Process message (future implementation)
        % For now just continue receiving
        ws_receive_loop(ConnectionId, WebSocket)
    ).

% wsSend builtin implementation
user:b(Pattern, wsSend, Message) :-
    format(user_error, "WebSocket send request: ~w -> ~w~n", [Pattern, Message]),
    
    % Find all matching connections
    findall(WebSocket, 
           (ws_connection(ID, Path, HeaderPairs, WebSocket),
            connection_matches_pattern(ID, Path, HeaderPairs, Pattern)),
           Connections),
    
    length(Connections, Count),
    format(user_error, "Found ~w matching connections~n", [Count]),
    
    % Send message to all matching connections
    send_ws_message(Connections, Message).

% Check if a connection matches a pattern
connection_matches_pattern(ID, Path, HeaderPairs, Pattern) :-
    % Convert headers to triples
    findall(p(Name, =, Value), member(Name=Value, HeaderPairs), HeaderTriples),
    
    % Create connection graph
    ConnectionGraph = graph([
        p(ID, a, wsConnection),
        p(ID, path, Path),
        p(ID, headers, graph(HeaderTriples))
    ]),
    
    % Use match builtin
    catch(
        b(ConnectionGraph, match, Pattern),
        Error,
        (format(user_error, "Match error: ~w~n", [Error]), fail)
    ).

% Send message to WebSocket connections
send_ws_message([], _) :- !.
send_ws_message([WebSocket|Rest], Message) :-
    % Determine message type and send
    (is_html_message(Message) ->
        % HTML message
        graph_to_html(Message, HTML),
        format(user_error, "Sending HTML message: ~w~n", [HTML]),
        ws_send(WebSocket, text(HTML))
    ;
        % JSON message
        graph_to_json(Message, JSON),
        format(user_error, "Sending JSON message: ~w~n", [JSON]),
        ws_send(WebSocket, json(JSON))
    ),
    % Continue with remaining connections
    send_ws_message(Rest, Message).

% Check if message is HTML format (has elements with 'a' predicate)
is_html_message(graph(Triples)) :-
    member(p(_, a, _), Triples), !.
is_html_message(_) :- fail.

% Convert graph to HTML
graph_to_html(graph(Triples), HTML) :-
    % Find root elements
    findall(Root, (
        member(p(Root, a, _), Triples),
        \+ member(p(_, child, Root), Triples)
    ), RootElements),
    
    % Generate HTML for each root element
    maplist(element_to_html(Triples), RootElements, HTMLs),
    atomic_list_concat(HTMLs, '\n', HTML).

% Convert element to HTML
element_to_html(Triples, Element, HTML) :-
    % Get element type
    memberchk(p(Element, a, Type), Triples),
    
    % Get attributes
    findall(Name-Value, (
        member(p(Element, Name, Value), Triples),
        Name \= a,
        Name \= child,
        Name \= text
    ), Attributes),
    
    % Format attributes
    format_attributes(Attributes, AttrStr),
    
    % Get content
    (memberchk(p(Element, text, Text), Triples) ->
        Content = Text
    ;
        % Get children
        findall(Child, member(p(Element, child, Child), Triples), Children),
        maplist(element_to_html(Triples), Children, ChildHTMLs),
        atomic_list_concat(ChildHTMLs, '', Content)
    ),
    
    % Create HTML tag
    format(string(HTML), '<~w~w>~w</~w>', [Type, AttrStr, Content, Type]).

% Format HTML attributes
format_attributes([], '').
format_attributes(Attrs, Result) :-
    maplist(format_attribute, Attrs, AttrStrs),
    atomic_list_concat(AttrStrs, ' ', Str),
    (Str = '' -> Result = '' ; format(string(Result), ' ~w', [Str])).

format_attribute(Name-Value, Str) :-
    format(string(Str), '~w="~w"', [Name, Value]).

% Convert graph to JSON
graph_to_json(graph(Triples), Dict) :-
    findall(Key-Value, (
        member(p(Key, =, RawValue), Triples),
        process_json_value(RawValue, Value)
    ), Pairs),
    dict_pairs(Dict, json, Pairs).

% Process JSON values
process_json_value(graph(Triples), NestedDict) :-
    !,  % Only for graph values
    findall(SubKey-SubValue, (
        member(p(SubKey, =, RawSubValue), Triples),
        process_json_value(RawSubValue, SubValue)
    ), SubPairs),
    dict_pairs(NestedDict, json, SubPairs).
process_json_value(Value, Value).  % Direct values