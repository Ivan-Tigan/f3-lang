:- module(server, [start_server/1]).

:- [f3_websocket].
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_header)).
:- use_module(library(http/json)).

% Basic server setup
:- dynamic server_port/1.
:- dynamic server_thread/1.

% HTTP handler for all routes
:- http_handler('/', handle_request, [prefix]).

% Main request handler
handle_request(Request) :-
    format(user_error, "Received request: ~w~n", [Request]),
    memberchk(path(PathAtom), Request),
    atom_string(PathAtom, Path),
    
    % Extract HTTP method
    memberchk(method(Method), Request),
    
    % CHECK FOR WEBSOCKET - ADD THESE LINES
    (   f3_websocket:is_websocket_request(Request),
        f3_websocket:handle_websocket_upgrade(Request, _)
    ->  % WebSocket handling complete, stop processing
        !  % Cut to prevent falling through to HTTP handling
    ;   % Not a WebSocket, continue with normal HTTP processing
        true
    ),
        format(user_error, "XXX before post/put handling : ~w ~w ~n", [Path, Method]),
    % Special handling for POST/PUT with body content
    (  (Method == post ; Method == put),
       memberchk(content_type(ContentType), Request)
    -> (  (ContentType == application/json ; sub_atom(ContentType, 0, _, _, 'application/json'))
       -> read_json_request(Request, JSONDict),
          format(user_error, "Parsed JSON body: ~w~n", [JSONDict]),
          json_body_to_triples(JSONDict, BodyTriples)
       ;  (ContentType == 'application/x-www-form-urlencoded' ; sub_atom(ContentType, 0, _, _, 'application/x-www-form-urlencoded'))
       -> read_form_request(Request, FormData),
          format(user_error, "Parsed form body: ~w~n", [FormData]),
          form_data_to_triples(FormData, BodyTriples)
       ;  format(user_error, "Unsupported content type: ~w~n", [ContentType]),
          BodyTriples = []
       ),
       
       % Build the triple pattern with body
       generate_request_id(RequestId),
          format(user_error, "Generated request id: ~w~n", [RequestId]),
       atom_string(PathAtom, Path),
       method_to_request_type(Method, RequestType),
       Pattern = graph([
           p(RequestId, a, RequestType),
           p(RequestId, path, Path),
           p(RequestId, body, graph(BodyTriples))
       ])
    ;  % Regular request without body
       request_to_triple_pattern(Request, Pattern)
    ),
    
    format(user_error, "Request triple pattern: ~w~n", [Pattern]),

    % Try to match request with a rule
    (   match_request_pattern(Pattern, Response)
    ->  % Found a matching rule
        format(user_error, "Found matching response pattern: ~w~n", [Response]),
        send_http_response(Response)
    ;   % No matching rule
        format(user_error, "No matching pattern for: ~w~n", [Path]),
        % Use the standard HTTP error mechanism
        throw(http_reply(not_found(Path)))
    ).

% Read JSON from request safely
read_json_request(Request, JSONDict) :-
    % Get the content length
    memberchk(content_length(ContentLength), Request),
    format(user_error, "Content length: ~w~n", [ContentLength]),
    
    % Get the input stream
    memberchk(input(InStream), Request),
    
    % Read raw JSON data
    read_string(InStream, ContentLength, RawJSON),
    format(user_error, "Raw JSON: ~w~n", [RawJSON]),
    
    % Parse the JSON
    catch(
        atom_json_dict(RawJSON, JSONDict, []),
        Error,
        (format(user_error, "JSON parse error: ~w~n", [Error]), JSONDict = _{})
    ).

% Convert JSON body to triples format
json_body_to_triples(JSONDict, Triples) :-
    dict_pairs(JSONDict, _, Pairs),
    maplist(json_pair_to_triple, Pairs, Triples).

% Read form data from request
read_form_request(Request, FormData) :-
    % Get the content length
    memberchk(content_length(ContentLength), Request),
    format(user_error, "Form content length: ~w~n", [ContentLength]),
    
    % Get the input stream
    memberchk(input(InStream), Request),
    
    % Read raw form data
    read_string(InStream, ContentLength, RawForm),
    format(user_error, "Raw form data: ~w~n", [RawForm]),
    
    % Parse the form data
    catch(
        parse_form_data(RawForm, FormData),
        Error,
        (format(user_error, "Form parse error: ~w~n", [Error]), FormData = [])
    ).

% Parse URL-encoded form data
parse_form_data(RawForm, FormData) :-
    split_string(RawForm, '&', '', Pairs),
    maplist(parse_form_pair, Pairs, FormData).

% Parse individual form field
parse_form_pair(Pair, Name=Value) :-
    split_string(Pair, '=', '', [NameEncoded, ValueEncoded]),
    uri_encoded(query_value, Name, NameEncoded),
    uri_encoded(query_value, Value, ValueEncoded).

% Convert form data to triples format
form_data_to_triples(FormData, Triples) :-
    maplist(form_pair_to_triple, FormData, Triples).

% Convert a form field to a triple
form_pair_to_triple(Name=Value, p(Name, =, Value)).

% Convert a JSON key-value pair to a triple
json_pair_to_triple(Key-Value, p(Key, =, ProcessedValue)) :-
    process_json_value(Value, ProcessedValue).

% Process different types of JSON values
process_json_value(Value, graph(NestedTriples)) :-
    is_dict(Value), !,
    dict_pairs(Value, _, Pairs),
    maplist(json_pair_to_triple, Pairs, NestedTriples).
process_json_value(Value, ProcessedList) :-
    is_list(Value), !,
    maplist(process_json_value, Value, ProcessedList).
process_json_value(Value, Value).

% Convert HTTP request to triple pattern for matching
request_to_triple_pattern(Request, TriplePattern) :-
    % Generate a unique request ID
    generate_request_id(RequestId),
    
    % Extract basic request components
    memberchk(method(Method), Request),
    memberchk(path(PathAtom), Request),
    atom_string(PathAtom, Path),
    
    % Determine request type based on HTTP method
    method_to_request_type(Method, RequestType),
    
    % Build basic pattern with request type and path
    BaseTriples = [
        p(RequestId, a, RequestType),
        p(RequestId, path, Path)
    ],
    
    % Extract and include query parameters if present
    (   memberchk(search(Search), Request),
        Search \= []
    ->  search_params_to_triples(Search, ParamTriples),
        TriplesWithParams = [p(RequestId, params, graph(ParamTriples))|BaseTriples]
    ;   TriplesWithParams = BaseTriples
    ),
    
    % Extract cookies if present
    (   memberchk(cookie(Cookies), Request)
    ->  maplist(cookie_to_triple, Cookies, CookieTriples),
        TriplesWithCookies = [p(RequestId, cookies, graph(CookieTriples))|TriplesWithParams]
    ;   TriplesWithCookies = TriplesWithParams
    ),
    
    % Extract other headers if needed
    findall(p(HeaderName, =, HeaderValue), 
            (member(HeaderTerm, Request),
             HeaderTerm =.. [HeaderName, HeaderValue],
             \+ member(HeaderName, [path_info, protocol, peer, pool, input, method, request_uri, path, http_version, cookie])
            ), 
            HeaderTriples),
    (   HeaderTriples \= []
    ->  AllTriples = [p(RequestId, headers, graph(HeaderTriples))|TriplesWithCookies]
    ;   AllTriples = TriplesWithCookies
    ),
    
    % Construct the final triple pattern
    TriplePattern = graph(AllTriples).

% Convert HTTP method to request type
method_to_request_type(get, get).
method_to_request_type(post, post).
method_to_request_type(put, put).
method_to_request_type(delete, delete).
method_to_request_type(_, request).

% Convert search parameters to triples
search_params_to_triples([], []).
search_params_to_triples([Name=Value|Rest], [p(Name, =, ValueString)|RestTriples]) :-
    atom_string(Value, ValueString),
    search_params_to_triples(Rest, RestTriples).

% Convert cookie to triple (convert values to strings for easier F3 matching)
cookie_to_triple(Name=Value, p(Name, =, ValueString)) :-
    atom_string(Value, ValueString).

% Match request pattern against defined rules
match_request_pattern(RequestPattern, Response) :-
    format(user_error, "Trying to match pattern: ~w~n", [RequestPattern]),
    
    % Find the corresponding response
    catch(
        p(RequestPattern, response, Response),
        Error,
        (format(user_error, "Response error: ~w~n", [Error]), fail)
    ).

% Send HTTP response based on matched rule
send_http_response(graph(ResponseTriples)) :-
    format(user_error, "Processing response: ~w~n", [ResponseTriples]),
    
    % Extract status code (default 200)
    (   memberchk(p(_, status, Status), ResponseTriples)
    ->  format(user_error, "Status: ~w~n", [Status])
    ;   Status = 200,
        format(user_error, "Default status: 200~n", [])
    ),
    
    % Extract headers with safe parsing
    format(user_error, "Extracting headers~n", []),
    findall(HeaderName-HeaderValue,
            (member(p(_, headers, graph(HeaderTriples)), ResponseTriples),
             member(p(HeaderName, =, HeaderValue), HeaderTriples),
             format(user_error, "Found header: ~q = ~q~n", [HeaderName, HeaderValue])
            ),
            HeaderPairs),
    format(user_error, "Header pairs: ~q~n", [HeaderPairs]),
    
    % Extract body content - optional, default to empty string
    (   memberchk(p(_, body, BodyContent), ResponseTriples)
    ->  true
    ;   BodyContent = ""
    ),
    
    % Determine content type
    format(user_error, "Finding content type in: ~q~n", [HeaderPairs]),
    (   member("Content-Type"-ContentType, HeaderPairs)
    ->  format(user_error, "Content type from headers: ~q~n", [ContentType])
    ;   ContentType = "text/html; charset=utf-8",
        format(user_error, "Using default content type~n", [])
    ),
    
    % Use body content directly as string
    format(user_error, "Using body content directly as string~n", []),
    ProcessedBody = BodyContent,
    
    % Send response headers
    format('Status: ~w~n', [Status]),
    format('Content-Type: ~w~n', [ContentType]),
    
    % Send any other headers
    forall(member(Name-Value, HeaderPairs),
           (Name \= "Content-Type" -> format('~w: ~w~n', [Name, Value]) ; true)),
    
    % Separate headers from body
    nl,
    
    % Send body
    format(user_error, "Sending response body~n", []),
    write(ProcessedBody),
    format(user_error, "Response complete~n", []).

% Process JSON value for output
process_json_pairs([], []).
process_json_pairs([Key-graph(Triples)|Rest], [Key-NestedJSON|ProcessedRest]) :-
    !,  % Cut to ensure we only handle graph case here
    % Handle nested graph structure
    findall(SubKey-SubValue, member(p(SubKey, =, SubValue), Triples), SubPairs),
    process_json_pairs(SubPairs, ProcessedSubPairs),
    dict_pairs(NestedJSON, json, ProcessedSubPairs),
    process_json_pairs(Rest, ProcessedRest).
process_json_pairs([Key-Value|Rest], [Key-Value|ProcessedRest]) :-
    % Handle direct values
    process_json_pairs(Rest, ProcessedRest).

% Generate unique request ID
generate_request_id(ID) :-
    get_time(Now),
    format(atom(TempID), 'req_~w', [Now]),
    atom_string(TempID, ID).

% Start HTTP server and keep it running
start_server(Port) :-
    % Stop existing server if any
    (   server_port(OldPort)
    ->  http_stop_server(OldPort, []),
        retractall(server_port(_))
    ;   true
    ),
    
    % Start new server
    format(user_error, 'Starting server on port ~w~n', [Port]),
    
    % Start HTTP server
    catch(
        http_server(http_dispatch, [port(Port)]),
        Error,
        (format(user_error, 'Server error: ~w~n', [Error]), fail)
    ),
    
    % Store port
    assertz(server_port(Port)),
    
    % Server started message
    format(user_error, 'Server started on port ~w and listening~n', [Port]),
    
    % Keep server running
    thread_get_message(_).