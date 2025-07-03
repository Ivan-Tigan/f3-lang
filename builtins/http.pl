:- module(http, []).


:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(uri)).
:- use_module(library(plunit)).

% Set test options to show output
:- set_test_options([silent(false)]).

:- multifile user:b/3.
:- multifile user:builtin/1.

:- use_module(cache).



% Convert graph of params to URL query string
params_to_query(graph(Params), QueryString) :-
    maplist(param_to_string, Params, ParamStrings),
    atomic_list_concat(ParamStrings, '&', QueryString).

param_to_string(p(Name,=,Value), ParamString) :-
    uri_encoded(query_value, Value, EncodedValue),
    format(atom(ParamString), '~w=~w', [Name, EncodedValue]).

% Convert graph of headers to options list
headers_to_options(graph(Headers), HeaderOptions) :-
    maplist(header_to_option, Headers, HeaderOptions).

header_to_option(p(Name,=,Value), request_header(Name=Value)).

% Convert graph body to JSON
body_to_json(graph(Body), JSON) :-
    maplist(body_pair_to_json, Body, Pairs),
    dict_pairs(JSON, _, Pairs).

body_pair_to_json(p(Name,=,graph(SubBody)), Name-SubJSON) :-
    !, % Cut for nested graphs
    body_to_json(graph(SubBody), SubJSON).
body_pair_to_json(p(Name,=,Value), Name-Value).

% Convert graph body to form data list
body_to_form(graph(Body), FormPairs) :-
    maplist(body_pair_to_form, Body, FormPairs).

body_pair_to_form(p(Name,=,Value), Name=Value).

% Convert JSON to graph format
json_to_graph(json(Fields), graph(GraphFields)) :-
    maplist(json_field_to_graph, Fields, GraphFields).

json_field_to_graph(Name=json(SubFields), p(Name,=,graph(GraphSubFields))) :-
    !, % Cut for when we have nested JSON
    maplist(json_field_to_graph, SubFields, GraphSubFields).
json_field_to_graph(Name='@'(true), p(Name,=,true)) :- !.
json_field_to_graph(Name='@'(false), p(Name,=,false)) :- !.
json_field_to_graph(Name='@'(null), p(Name,=,null)) :- !.
json_field_to_graph(Name=Value, p(Name,=,ValueString)) :-
    atom(Value), !,
    atom_string(Value, ValueString).
json_field_to_graph(Name=Value, p(Name,=,Value)).

user:builtin(fetch).
% Main predicate to handle HTTP requests with caching
user:b(Request, fetch, Response) :-
    % Extract components from Request
    Request = graph(RequestProps),
    memberchk(p(_, url, BaseURL), RequestProps),
    memberchk(p(_, a, Method), RequestProps),
    
    % Extract optional params
    (memberchk(p(_, params, Params), RequestProps) ->
        params_to_query(Params, QueryString),
        format(atom(URL), '~w?~w', [BaseURL, QueryString])
    ;   URL = BaseURL
    ),
    
    % Extract optional headers
    (memberchk(p(_, headers, Headers), RequestProps) ->
        headers_to_options(Headers, HeaderOptions)
    ;   HeaderOptions = []
    ),
    
    % Handle different HTTP methods with error handling
    catch(
        (Method = post ->
            (memberchk(p(_, body, Body), RequestProps) ->
                % Check if we have form data by looking at content-type
                (memberchk(request_header("Content-Type"="application/x-www-form-urlencoded"), HeaderOptions) ->
                    % Handle form data
                    body_to_form(Body, FormData),
                    append(HeaderOptions, [form(FormData), status_code(_)], Options),
                    http_post(URL, form(FormData), ResponseData, Options)
                ;   % Handle JSON data
                    body_to_json(Body, JSONBody),
                    append(HeaderOptions, [json(JSONBody), status_code(_)], Options),
                    http_post(URL, json(JSONBody), ResponseData, Options)
                )
            ;   % POST without body
                append(HeaderOptions, [status_code(_)], Options),
                http_post(URL, '', ResponseData, Options)
            )
        ; Method = get ->
            append(HeaderOptions, [status_code(_)], Options),
            http_get(URL, ResponseData, Options)
        ),
        Error,
        (format(user_error, 'HTTP Error: ~w~n', [Error]), 
         ResponseData = json([error=Error]))
    ),
    
    % Convert response to graph format
    (is_dict(ResponseData) ->
        json_to_graph(ResponseData, ResponseBody)
    ; ResponseData = json(_) ->
        json_to_graph(ResponseData, ResponseBody)
    ;   ResponseBody = graph([p(text, =, ResponseData)])
    ),
    
    % Construct response
    Response = graph([
        p(response, headers, graph([p(content_type,=,'application/json')])),
        p(response, body, ResponseBody)
    ]).
    

% Tests
:- begin_tests(http_client).

test(post_request_nested) :-
    Request = graph([
        p(1, a, post),
        p(1, url, "https://httpbin.org/post"),
        p(1, params, graph([p(param1,=,1),p(param2,=,3)])),
        p(1, body, graph([
            p(user,=,graph([
                p(name,=,"John"),
                p(age,=,30),
                p(address,=,graph([
                    p(street,=,"123 Main St"),
                    p(city,=,"New York")
                ]))
            ])),
            p(settings,=,graph([
                p(enabled,=,true),
                p(theme,=,"dark")
            ]))
        ])),
        p(1, headers, graph([
            p("Content-Type",=,"application/json"),
            p("X-Custom-Header",=,"test-value")
        ]))
    ]),
    b(Request, fetch, Response),
    Response = graph([
        p(response, headers, _),
        p(response, body, ResponseBody)
    ]),
 format('~n~nRequest:~n~w~n~n', [Request]),
    format('~nResponse:~n~w~n~n', [Response]),
    ResponseBody = graph(Fields),
    memberchk(p(json,=,graph(SentData)), Fields),
    memberchk(p(user,=,graph(UserData)), SentData),
    memberchk(p(name,=,"John"), UserData),
    memberchk(p(address,=,graph(AddressData)), UserData),
    memberchk(p(city,=,"New York"), AddressData).

test(get_request) :-
    Request = graph([
        p(1, a, get),
        p(1, url, "https://httpbin.org/get"),
        p(1, params, graph([
            p(q,=,"test+query"),
            p(page,=,1),
            p(sort,=,"desc")
        ])),
        p(1, headers, graph([
            p("Accept",=,"pplication/json"),
            p("X-Test-Header",=,"test-value")
        ]))
    ]),
 format('~n~nRequest:~n~w~n~n', [Request]),
    b(Request, fetch, Response),
    format('~nResponse:~n~w~n~n', [Response]),

    Response = graph([
        p(response, headers, _),
        p(response, body, ResponseBody)
    ]),
    ResponseBody = graph(Fields),
    memberchk(p(args,=,graph(Args)), Fields),
    memberchk(p(q,=,"test+query"), Args),
    memberchk(p(headers,=,graph(Headers)), Fields),
    memberchk(p('X-Test-Header',=,"test-value"), Headers).

test(post_form_request) :-
    Request = graph([
        p(1, a, post),
        p(1, url, "https://httpbin.org/post"),
        p(1, headers, graph([
            p("Content-Type",=,"application/x-www-form-urlencoded")
        ])),
        p(1, body, graph([
            p("grant_type",=,"authorization_code"),
            p("code",=,"test_code_123"),
            p("client_id",=,"test_client"),
            p("client_secret",=,"secret123")
        ]))
    ]),
    format('~n~nForm POST Request:~n~w~n~n', [Request]),
    b(Request, fetch, Response),
    format('~nForm POST Response:~n~w~n~n', [Response]),
    Response = graph([
        p(response, headers, _),
        p(response, body, ResponseBody)
    ]),
    ResponseBody = graph(Fields),
    memberchk(p(form,=,graph(FormData)), Fields),
    % Verify the form data was sent correctly
    memberchk(p(grant_type,=,"authorization_code"), FormData),
    memberchk(p(client_id,=,"test_client"), FormData).

:- end_tests(http_client).
:- set_test_options([silent(false)]).

:-set_prolog_flag(plunit_output, always).
% Run tests and halt
% :- (run_tests -> halt(0) ; halt(1)).