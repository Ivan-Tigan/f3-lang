:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

fetch_json(URL, JSON) :-
    setup_call_cleanup(
        http_open(URL, Stream, []),
        json_read_dict(Stream, JSON),
        close(Stream)
    ).
% Base case: when Path is [jsonGet], return the current JSON as the value.
b(Value, [jsonGet], Value) :- !.

% If the path is [jsonGet|Rest] and the current JSON is a dictionary, use get_dict to extract the next value.
b(JSON, [jsonGet, Key|Rest], Value) :-
    get_dict(Key, JSON, Next),
    b(Next, [jsonGet|Rest], Value), !.

% If the path includes an integer index (e.g., for lists), use nth0 to extract the element.
b(List, [jsonGet, Index|Rest], Value) :-
    integer(Index),
    nth0(Index, List, Next),
    b(Next, [jsonGet|Rest], Value), !.
