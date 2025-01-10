:- debug(parser).
% Map to track variables during parse
:- dynamic parse_vars/1.
:- dynamic p/3.
:- multifile p/3.

:- initialization(assertz(parse_vars([]))).

reset_parse_vars :-
    retractall(parse_vars(_)).
    % assertz(parse_vars()).

% get_or_create_var(Name, Var) :-
%     (parse_vars(Name-Var) ->
%         true  % Return existing var
%     ;   
%         Var = Y,  % Create new var
%         % retractall(parse_vars(_)),
%         assertz(parse_vars(Name-Var))
%     ).

parse_triples(String, Result) :-
    reset_parse_vars,
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
    blanks,
    statement(First),
    remaining_top_statements(Rest),
    { append(First, Rest, Statements) }.

remaining_top_statements([]) --> [].
remaining_top_statements(Statements) -->
    statement(First),
    remaining_top_statements(Rest),
    { append(First, Rest, Statements) }.

% A statement ends with a period
statement(Triples) -->
    blanks,
    triple(Triples),
    ".",
    blanks.

% Regular triple parser
triple([p(S,P,O)|More]) -->
    node(S),
    blanks,
    predicate_objects(S, [p(S,P,O)|More]).

% Handle predicates and their objects
predicate_objects(S, [p(S,P,O)|More]) -->
    node(P),
    blanks,
    objects(S, P, O, More).

% Handle multiple objects for a predicate
objects(S, P, O, More) -->
    node(O),
    blanks,
    (
        ",",
        blanks,
        objects(S, P, NextO, RestMore),
        { More = [p(S,P,NextO)|RestMore] }
    |
        ";",
        blanks,
        predicate_objects(S, More)
    |
        { More = [] }
    ).

% Parse any type of node
node(Node) -->
    (
        quoted_string(Node)
    |   
        number(Node)
    |   
        symbol(Node)
    |   
        identifier(Node)
    |   
        list_node(Node)
    |    
        nested_graph(Node)
    ).
% New predicate to parse double-colon separated lists
colon_list_node(Items) -->
    node(FirstItem),
    blanks,
    "::",
    blanks,
    colon_list_items(RestItems),
    { Items = [FirstItem|RestItems] }.

colon_list_items([Item|Rest]) -->
    node(Item),
    blanks,
    ( 
        "::",
        blanks,
        colon_list_items(Rest)
    |
        { Rest = [] }
    ).
symbol(Symbol) -->
    symbol_chars(Chars),
    symbol_end,
    { atom_codes(Symbol, Chars) }.

symbol_chars([C|Cs]) -->
    [C],
    { symbol_char(C) },
    symbol_chars_rest(Cs).

symbol_chars_rest([C|Cs]) -->
    [C],
    { symbol_char(C) },
    symbol_chars_rest(Cs).
symbol_chars_rest([]) --> [].

symbol_char(C) :- 
    member(C, [
        0'-, 0'>, 0'<, 0'=, 0'+, 0'*, 0'/, 0'\\, 0'^, 0'~, 
        0'#, 0'$, 0'@, 0'?, 0'|, 0':, 0'&, 0'%,
        0'!, 0';, 0'+
    ]).

symbol_end -->
    peek(C),
    { \+ symbol_char(C) }.

list_node(Items) -->
    "[",
    blanks,
    list_items(Items),
    "]".

list_items([Item|Rest]) -->
    node(Item),
    blanks,
    (
        list_items(Rest)
    |
        { Rest = [] }
    ).

nested_graph(graph(Triples)) -->
    "{",
    blanks,
    graph_statements(Triples),
    blanks,
    "}".

graph_statements(Statements) -->
    triple(First),
    ".",
    blanks,
    remaining_graph_statements(Rest),
    { append(First, Rest, Statements) }.

remaining_graph_statements([]) --> [].
remaining_graph_statements(Rest) -->
    triple(First),
    ".",
    blanks,
    remaining_graph_statements(More),
    { append(First, More, Rest) }.

identifier(Id) --> 
    [C],
    { code_type(C, alpha) },
    id_rest(Cs),
    identifier_end,
    { atom_codes(Name, [C|Cs]),
      (char_type(C, upper) ->
    %   term_to_atom(UnquotedName, Name),
      Id = '$VAR'(Name), !
    %     write_canonical(Id), write(' for '), 
    %   write_canonical(Name), 
    %    nl


         
      ;
         Id = Name
      )
    }.

id_rest([C|Cs]) --> 
    [C], { code_type(C, alnum); C = 0'_ }, 
    id_rest(Cs).
id_rest([]) --> [].

identifier_end -->
    peek(C),
    { \+ (code_type(C, alnum); C = 0'_) }.

peek(C, [C|Rest], [C|Rest]).

number(N) -->
    digits(D),
    { number_codes(N, D) }.

digits([D|Ds]) -->
    [D],
    { code_type(D, digit) },
    (digits(Ds) ; { Ds = [] }).

quoted_string(String) -->
    "\"",
    string_chars(Chars),
    "\"",
    { string_chars(String, Chars) },
    blanks.

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
    test_todo,
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
    % !.
test_todo :- 
    writeln("Testing todo"),
    parse_triples("
        html attribute class, href, id, l1, l2, l3; element a, div.

        homePage a page; path \"\"; content homePageContent.
        homePageContent a div; class \"flex w-full h-full justify-center\"; child [homePageContent inner].
        [homePageContent inner] a div; class \"flex flex-col justify-start p-2\".

    
        [db \"world\"] hasGraph {
            \"user_1\" a user; name \"Bob\"; hasTask \"user_1/task_1\", \"user_1/task_2\", \"user_1/task_3\".
            \"user_1/task_1\" name \"Bob's first task\".
            \"user_1/task_2\" name \"Bob's second task\".
            \"user_1/task_3\" name \"Bob's third task\".
        }.

        { 
            [db \"world\"] hasSubGraph { UID a user; name Name. }.
            [\"user/\" UID] sconcat Path.
        } => {
            [homePageContent inner] child [homePageContent inner Path].
            [homePageContent inner Path] a a; href Path; text Name.
            [userpage UID Name] a page; path Path; content [userpage UID Name content].
            [userpage UID Name content] a div; class \"flex w-full h-full justify-center\"; child [userpage UID Name content inner].
            [userpage UID Name content inner] a div; class \"flex flex-col justify-start p-2\".
        }.

        { 
            a b c.
            [db \"world\"] hasSubGraph { UID a user; name Name; hasTask T. T name TName. }.
            } => {
            [userpage UID Name content inner] child [userpage UID Name content inner T].
            [usepage UID Name content inner T] a div; text TName. 
        }.


    ", Triples),
    writeln(Triples).

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
    parse_triples("bob friends [alice carol 123 \"david\"].", Triples),
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
test_colon_lists :-
    writeln('Testing colon-separated lists...'),
    parse_triples("bob friends alice::carol::david.", T1),
    writeln(T1),
    parse_triples("x path 1::2::3::4.", T2),
    writeln(T2),
    parse_triples("data values \"hello\"::123::world.", T3),
    writeln(T3).
% :- nodebug(parser).
% :- test_all.