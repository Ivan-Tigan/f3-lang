:- debug(parser).

parse_triples(String, Result) :-
    string_codes(String, Codes),
    (phrase(top_statement(Result), Codes) ->
        true
    ;
        debug_parse(Codes),
        fail
    ).

debug_parse(Codes) :-
    string_codes(RestString, Codes),
    format('Failed to parse: ~w~n', [RestString]).

% Top level - handle multiple statements
top_statement(Statements) -->
    { debug(parser, "Starting top level parse", []) },
    statement(First),
    { debug(parser, "Got statement: ~w", [First]) },
    remaining_top_statements(Rest),
    { append(First, Rest, Statements) },
    { debug(parser, "Completed top level parse", []) }.

remaining_top_statements([]) --> 
    { debug(parser, "No more top statements", []) }.
remaining_top_statements(Statements) -->
    statement(First),
    { debug(parser, "Got next top statement: ~w", [First]) },
    remaining_top_statements(Rest),
    { append(First, Rest, Statements) }.

% A statement ends with a period
statement(Triples) -->
    triple(Triples),
    ".",
    blanks,
    !.  % Once we see a period, commit to this interpretation

% Regular triple parser
triple([p(S,P,O)|More]) -->
    { debug(parser, "Starting triple", []) },
    node(S),
    { debug(parser, "Got subject: ~w", [S]) },
    blanks,
    predicate_objects(S, [p(S,P,O)|More]).

% Handle predicates and their objects
predicate_objects(S, [p(S,P,O)|More]) -->
    node(P),
    { debug(parser, "Got predicate: ~w", [P]) },
    blanks,
    objects(S, P, O, More).

% Handle multiple objects for a predicate
objects(S, P, O, More) -->
    node(O),
    { debug(parser, "Got object: ~w", [O]) },
    blanks,
    (
        ",",
        blanks,
        { debug(parser, "Found comma, parsing more objects", []) },
        objects(S, P, NextO, RestMore),
        { More = [p(S,P,NextO)|RestMore] }
    |
        ";",
        blanks,
        { debug(parser, "Found semicolon, parsing new predicate", []) },
        predicate_objects(S, More)
    |
        { More = [] }
    ).

% Parse any type of node
node(Node) -->
    { debug(parser, "Parsing node", []) },
    (
        quoted_string(Node),
        !  % Once we start a string, commit to it
    |   
        number(Node),
        !  % Once we start a number, commit to it
    |   
        symbol(Node),    % Add this new option
        !  % Once we start a symbol, commit to it
    |   
        identifier(Node),
        !  % Once we start an identifier, commit to it
    |   
        list_node(Node),
        !  % Once we start a list, commit to it
    |   
        nested_graph(Node),
        !  % Once we start a graph, commit to it
    ).
symbol(Symbol) -->
    symbol_chars(Chars),
    symbol_end,
    { atom_codes(Symbol, Chars) },
    { debug(parser, "Parsed symbol: ~w", [Symbol]) },
    !.

symbol_chars([C|Cs]) -->
    [C],
    { symbol_char(C) },
    symbol_chars_rest(Cs).
symbol_chars_rest([C|Cs]) -->
    [C],
    { symbol_char(C) },
    symbol_chars_rest(Cs).
symbol_chars_rest([]) --> [].

% Define which characters can be part of a symbol
symbol_char(C) :- 
    member(C, [
        0'-, 0'>, 0'<, 0'=, 0'+, 0'*, 0'/, 0'\\, 0'^, 0'~, 
        0'#, 0'$, 0'@, 0'?, 0'|, 0':, 0'&, 0'%,
        0'!, 0';, 0'+
    ]).

% A symbol must end with a proper boundary
symbol_end -->
    peek(C),
    { \+ symbol_char(C) }.
% Parse a list node
list_node(list(Items)) -->
    "[",
    blanks,
    list_items(Items),
    "]",
    !.  % Once we complete a list, commit to it

list_items([Item|Rest]) -->
    node(Item),
    blanks,
    (
        ",",
        blanks,
        list_items(Rest)
    |
        { Rest = [] }
    ).

% Parse a nested graph
nested_graph(graph(Triples)) -->
    "{",
    { debug(parser, "Starting nested graph", []) },
    blanks,
    graph_statements(Triples),
    blanks,
    "}",
    { debug(parser, "Completed nested graph: ~w", [Triples]) },
    !.

% Parse statements within a graph - modified to handle triples directly
graph_statements(Statements) -->
    { debug(parser, "Starting graph statements", []) },
    triple(First),
    ".",
    blanks,
    !,
    remaining_graph_statements(Rest),
    { append(First, Rest, Statements) }.  % Key change: append the actual triples

% Remaining graph statements
remaining_graph_statements([]) -->
    { debug(parser, "No more graph statements", []) }.
remaining_graph_statements(Rest) -->
    triple(First),
    ".",
    blanks,
    !,
    remaining_graph_statements(More),
    { append(First, More, Rest) }.  % Key change: append the actual triples

% Basic lexical rules
identifier(Id) --> 
    [C], { code_type(C, alpha) },
    id_rest(Cs),
    identifier_end,
    { atom_codes(Id, [C|Cs]) },
    { debug(parser, "Parsed identifier: ~w", [Id]) },
    !.  % Once we complete an identifier, commit to it

id_rest([C|Cs]) --> 
    [C], { code_type(C, alnum); C = 0'_ }, 
    id_rest(Cs).
id_rest([]) --> [].

% An identifier must end with a proper boundary
identifier_end -->
    peek(C),
    { \+ (code_type(C, alnum); C = 0'_) }.

% Peek at next character without consuming it
peek(C, [C|Rest], [C|Rest]).

number(N) -->
    digits(D),
    { number_codes(N, D) },
    { debug(parser, "Parsed number: ~w", [N]) },
    !.  % Once we complete a number, commit to it

digits([D|Ds]) -->
    [D],
    { code_type(D, digit) },
    (digits(Ds) ; { Ds = [] }).

quoted_string(String) -->
    "\"",
    string_chars(Chars),
    "\"",
    { string_chars(String, Chars) },
    { debug(parser, "Parsed string: ~w", [String]) },
    blanks,
    !.  % Once we complete a string, commit to it

string_chars([]) --> [].
string_chars([C|Cs]) -->
    [C],
    { C \= 0'" },
    string_chars(Cs).

blanks --> [C], { code_type(C, space) }, blanks.
blanks --> [].
% Tests
test_all :-
    writeln('Running all tests...'),
    test_basic_triple,
    test_multiple_objects,
    test_multiple_predicates,
    test_graph,
    test_nested_graph,
    test_nested_graphs,
    test_mixed_nodes,
    test_list_nodes,
    test_symbols,
    writeln('All tests passed!'),
    halt.
test_nested_graphs :-
    writeln('Testing deeply nested graphs...'),
    parse_triples("db hasGraph { g1 contains { \"user_1\" name \"bob\". }. g2 type \"group\". }.", Triples),
    writeln(Triples),
    
    writeln('Testing multiple nested graphs...'),
    parse_triples("db hasGraph { g1 data { a b c. }; metadata { x y z. }. }.", Triples2),
    writeln(Triples2).
test_basic_triple :-
    writeln('Testing basic triple...'),
    parse_triples("bob name \"Robert\".", Triples),
    writeln(Triples).

test_multiple_objects :-
    writeln('Testing multiple objects...'),
    parse_triples("bob likes alice, carol, david.", Triples),
    writeln(Triples).

test_multiple_predicates :-
    writeln('Testing multiple predicates...'),
    parse_triples("bob likes alice; hates carol; knows david.", Triples),
    writeln(Triples).

test_graph :-
    writeln('Testing graph...'),
    parse_triples("db hasGraph { \"user_1\" name \"bob\". \"user_1\" age 20. }.", Triples),
    writeln(Triples).

test_nested_graph :-
    writeln('Testing nested graph...'),
    parse_triples("db hasGraph { g1 contains { \"user_1\" name \"bob\". }. }.", Triples),
    writeln(Triples).

test_mixed_nodes :-
    writeln('Testing mixed node types...'),
    parse_triples("123 hasString \"test\". \"test\" hasNumber 456.", Triples),
    writeln(Triples).

test_list_nodes :-
    writeln('Testing list nodes...'),
    parse_triples("bob friends [alice, carol, 123, \"david\"].", Triples),
    writeln(Triples).
test_symbols :-
    writeln('Testing symbols...'),
    % Basic operators
    parse_triples("a -> b.", T1),
    writeln(T1),
    parse_triples("x <-> y.", T2),
    writeln(T2),
    
    % Comparison operators
    parse_triples("p >= q.", T3),
    writeln(T3),
    parse_triples("m <= n.", T4),
    writeln(T4),
    
    % Logical operators
    parse_triples("x || y.", T5),
    writeln(T5),
    parse_triples("a && b.", T6),
    writeln(T6),
    
    % Special operators
    parse_triples("f :- g.", T7),
    writeln(T7),
    parse_triples("x |> y.", T8),
    writeln(T8).

:- nodebug(parser).
% :- test_all.