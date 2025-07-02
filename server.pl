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
    
    % Special handling for POST/PUT with JSON
    (  (Method == post ; Method == put),
       memberchk(content_type(ContentType), Request),
       (ContentType == application/json ; sub_atom(ContentType, 0, _, _, 'application/json'))
    -> read_json_request(Request, JSONDict),
       format(user_error, "Parsed JSON body: ~w~n", [JSONDict]),
       
       % Convert JSON body to triples
       json_body_to_triples(JSONDict, BodyTriples),
       
       % Build the triple pattern with JSON body
       generate_request_id(RequestId),
       atom_string(PathAtom, Path),
       method_to_request_type(Method, RequestType),
       % Build request pattern with body
       Pattern = graph([
           p(RequestId, a, RequestType),
           p(RequestId, path, Path),
           p(RequestId, body, graph(BodyTriples))
       ])
    ;  % Regular request without JSON body
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
        AllTriples = [p(RequestId, params, graph(ParamTriples))|BaseTriples]
    ;   AllTriples = BaseTriples
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
search_params_to_triples([Name=Value|Rest], [p(Name, =, Value)|RestTriples]) :-
    search_params_to_triples(Rest, RestTriples).

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
    
    % Extract body content - expect direct string
    memberchk(p(_, body, BodyContent), ResponseTriples),
    
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

% Generate JSON response from triples
generate_json_response(graph(Triples), JSONString) :-
    format(user_error, "Converting triples to JSON dict~n", []),
    
    % Extract key-value pairs
    findall(Key-Value, member(p(Key, =, Value), Triples), Pairs),
    format(user_error, "Extracted key-value pairs: ~q~n", [Pairs]),  % ~q shows quotes
    
    % Process nested values
    process_json_pairs(Pairs, ProcessedPairs),
    format(user_error, "Processed pairs: ~q~n", [ProcessedPairs]),  % ~q shows quotes
    
    % Create JSON dict
    dict_pairs(JSON, json, ProcessedPairs),
    format(user_error, "Created JSON dict: ~w~n", [JSON]),
    
    % Write to string
    with_output_to(string(JSONString), 
                   json_write(current_output, JSON, [width(0)])),
    format(user_error, "Generated JSON string (length: ~w)~n", [string_length(JSONString, _)]).

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

% Generate HTML response from triples
generate_html_response(graph(Triples), HTML) :-
    % Find root elements
    findall(Root, (
        member(p(Root, a, _), Triples),
        \+ member(p(_, child, Root), Triples)
    ), RootElements),
    format(user_error, "HTML root elements: ~w~n", [RootElements]),
    
    % Generate HTML for each root element
    maplist(element_to_html(Triples), RootElements, ElementHTMLs),
    
    % Combine all HTML
    atomic_list_concat(ElementHTMLs, '\n', HTML),
    format(user_error, "Generated HTML (length: ~w)~n", [string_length(HTML, _)]).
generate_html_response(Text, Text) :-
    (string(Text) ; atom(Text)),
    format(user_error, "Direct text content (length: ~w)~n", [string_length(Text, _)]).

% Convert an element to HTML string
element_to_html(Triples, Element, HTML) :-
    % Get element type
    memberchk(p(Element, a, ElementType), Triples),
    
    % Get element attributes
    findall(Attr-Value, (
        member(p(Element, Attr, Value), Triples),
        Attr \= a,
        Attr \= child,
        Attr \= text
    ), Attributes),
    
    % Format attributes
    format_attributes(Attributes, AttributesStr),
    
    % Get content (text or children)
    (   memberchk(p(Element, text, Text), Triples)
    ->  Content = Text
    ;   findall(Child, member(p(Element, child, Child), Triples), Children),
        maplist(element_to_html(Triples), Children, ChildrenHTML),
        atomic_list_concat(ChildrenHTML, '', Content)
    ),
    
    % Format HTML tag
    format(string(HTML), '<~w~w>~w</~w>', [ElementType, AttributesStr, Content, ElementType]).

% Format attributes for HTML
format_attributes([], '').
format_attributes(Attributes, AttributesStr) :-
    maplist(format_attribute, Attributes, AttrStrings),
    atomic_list_concat(AttrStrings, ' ', AttrStr),
    (   AttrStr = ''
    ->  AttributesStr = ''
    ;   format(string(AttributesStr), ' ~w', [AttrStr])
    ).

format_attribute(Name-Value, AttributeStr) :-
    format(string(AttributeStr), '~w="~w"', [Name, Value]).

% Generate unique request ID
generate_request_id(ID) :-
    get_time(Now),
    format(atom(TempID), 'req_~w', [Now]),
    atom_string(TempID, TempIDStr),
    sub_atom(TempIDStr, 0, 8, _, ID).

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