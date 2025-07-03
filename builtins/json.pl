:- module(json_builtin, []).

:- use_module(library(http/json)).
:- use_module(library(plunit)).

:- multifile user:b/3.
:- multifile user:builtin/1.

% Set test options to show output
:- set_test_options([silent(false)]).

user:builtin(parseJson).
user:builtin(jsonToString).

% Convert JSON string to graph format (bidirectional)
user:b(JsonString, parseJson, Graph) :-
    atom_json_term(JsonString, JsonTerm, [value_string_as(string)]),
    json_to_graph(JsonTerm, Graph).

% Convert graph to JSON string (bidirectional)
user:b(Graph, jsonToString, JsonString) :-
    graph_to_json(Graph, JsonTerm),
    atom_json_term(JsonString, JsonTerm, [value_string_as(string)]).

% Helper: Convert SWI-Prolog JSON term to F3 graph format
json_to_graph(json(Fields), graph(GraphFields)) :-
    maplist(json_field_to_graph, Fields, GraphFields).

json_field_to_graph(Name=json(SubFields), p(Name,=,graph(GraphSubFields))) :-
    !, % Cut for when we have nested JSON objects
    maplist(json_field_to_graph, SubFields, GraphSubFields).
json_field_to_graph(Name='@'(true), p(Name,=,true)) :- !.
json_field_to_graph(Name='@'(false), p(Name,=,false)) :- !.
json_field_to_graph(Name='@'(null), p(Name,=,null)) :- !.
json_field_to_graph(Name=Value, p(Name,=,Value)).

% Helper: Convert F3 graph format to SWI-Prolog JSON term
graph_to_json(graph(GraphFields), json(Fields)) :-
    maplist(graph_field_to_json, GraphFields, Fields).

graph_field_to_json(p(Name,=,graph(GraphSubFields)), Name=json(SubFields)) :-
    !, % Cut for nested graph objects
    maplist(graph_field_to_json, GraphSubFields, SubFields).
graph_field_to_json(p(Name,=,true), Name='@'(true)) :- !.
graph_field_to_json(p(Name,=,false), Name='@'(false)) :- !.
graph_field_to_json(p(Name,=,null), Name='@'(null)) :- !.
graph_field_to_json(p(Name,=,Value), Name=Value).

% Tests
:- begin_tests(json_builtin).

test(parse_simple_json) :-
    JsonString = '{"name": "John", "age": 30}',
    b(JsonString, parseJson, Graph),
    Graph = graph([p(name,=,"John"), p(age,=,30)]).

test(parse_nested_json) :-
    JsonString = '{"user": {"name": "Alice", "age": 25}, "active": true}',
    b(JsonString, parseJson, Graph),
    Graph = graph([
        p(user,=,graph([p(name,=,"Alice"), p(age,=,25)])),
        p(active,=,true)
    ]).

test(json_to_string_simple) :-
    Graph = graph([p(name,=,"Bob"), p(age,=,35)]),
    b(Graph, jsonToString, JsonString),
    % Parse it back to verify it's valid JSON
    b(JsonString, parseJson, ParsedBack),
    Graph = ParsedBack.

test(json_to_string_nested) :-
    Graph = graph([
        p(user,=,graph([p(name,=,"Carol"), p(email,=,"carol@example.com")])),
        p(status,=,"active")
    ]),
    b(Graph, jsonToString, JsonString),
    % Parse it back to verify roundtrip
    b(JsonString, parseJson, ParsedBack),
    Graph = ParsedBack.

test(jwt_payload_example) :-
    % Real JWT payload example
    JwtPayload = '{"sub":"1234567890","name":"John Doe","iat":1516239022}',
    b(JwtPayload, parseJson, Graph),
    Graph = graph([
        p(sub,=,"1234567890"),
        p(name,=,"John Doe"), 
        p(iat,=,1516239022)
    ]).

test(bidirectional_round_trip) :-
    % Test full bidirectional functionality
    OriginalGraph = graph([
        p(userId,=,"user123"),
        p(profile,=,graph([
            p(name,=,"Test User"),
            p(preferences,=,graph([p(theme,=,"dark")]))
        ]))
    ]),
    b(OriginalGraph, jsonToString, JsonString),
    b(JsonString, parseJson, ParsedGraph),
    OriginalGraph = ParsedGraph.

:- end_tests(json_builtin).
:- set_test_options([silent(false)]).

:- set_prolog_flag(plunit_output, always).

% Run tests and halt
% :- (run_tests -> halt(0) ; halt(1)).