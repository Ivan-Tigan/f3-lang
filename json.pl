:- use_module(library(http/json)).

% For generating unique IDs
:- dynamic next_id/1.
:- assertz(next_id(1)).

% Simple UUID generator (just increments a number)
uuid(Id) :-
    retract(next_id(N)),
    format(atom(Id), 'obj_~w', [N]),
    N1 is N + 1,
    assertz(next_id(N1)).

% Main conversion predicate
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

% Test predicates
test1 :-
    Dict = _{
        name: "John",
        age: 30,
        address: _{
            street: "123 Main",
            city: "Boston"
        },
        hobbies: ["reading", "gaming"]
    },
    json_to_triples(Dict, Guid, Triples),
    format('Guid: ~w~n', [Guid]),
    format('Triples:~n'),
    print_triples(Triples).

test2 :-
    Dict = _{
        users: [
            _{name: "John", age: 30},
            _{name: "Jane", age: 25}
        ],
        settings: _{
            theme: "dark",
            notifications: _{
                email: true,
                push: false
            }
        }
    },
    json_to_triples(Dict, Guid, Triples),
    format('Guid: ~w~n', [Guid]),
    format('Triples:~n'),
    print_triples(Triples).

% Helper to pretty print triples
print_triples([]).
print_triples([p(S,P,O)|Rest]) :-
    write_canonical(S), write(" "),
    write_canonical(P), write(" "),
    write_canonical(O), 
    nl,
    print_triples(Rest).

% Parse JSON string and convert to triples
json_string_to_triples(JsonString, Guid, Triples) :-
    atom_json_dict(JsonString, Dict, []),
    json_to_triples(Dict, Guid, Triples).

% Test with JSON string
test_string :-
    JsonString = '{"name": "John", "age": 30, "address": {"street": "123 Main", "city": "Boston"}}',
    json_string_to_triples(JsonString, Guid, Triples),
    format('Guid: ~w~n', [Guid]),
    format('Triples:~n'),
    print_triples(Triples).

:- test1.
:- test2.
:- test_string.