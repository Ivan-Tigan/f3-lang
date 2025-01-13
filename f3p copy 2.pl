:- debug(parser).
:- dynamic parse_vars/1.
:- dynamic p/3.
:- multifile p/3.

:- initialization(assertz(parse_vars([]))).

reset_parse_vars :-
   retractall(parse_vars(_)).

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

statement(Triples) -->
   blanks,
   triple(Triples),
   ".",
   blanks.

triple([p(S,P,O)|More]) -->
   node(S),
   blanks,
   predicate_objects(S, [p(S,P,O)|More]).

   predicate_objects(S, [p(S,P,O)|More]) -->
    node(P),
    blanks,
    objects(S, P, O, RestMore),
    (
        ";",
        blanks,
        node(NextP),
        blanks,
        objects(S, NextP, NextO, FinalMore),
        { append(RestMore, [p(S,NextP,NextO)|FinalMore], More) }
    |
        { More = RestMore }
    ).
% Handle multiple objects for a predicate
objects(S, P, O, More) -->
    (
        "{",
        blanks,
        nested_triple_content([FirstTriple|RestTriples]),
        blanks,
        ".",
        blanks, 
        remaining_nested_triples(MoreTriples),
        "}",
        { 
            O = FirstTriple,
            % Transform all nested triples to include outer S and P
            transform_nested_triples(RestTriples, S, P, TransformedRest),
            transform_nested_triples(MoreTriples, S, P, TransformedMore),
            append(TransformedRest, TransformedMore, More)
        }
    |
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
        )
    ).
 
% Helper predicate to transform nested triples
transform_nested_triples([], _, _, []).
transform_nested_triples([p(InnerS,P,O)|Rest], S, OuterP, [p(S,OuterP,p(InnerS,P,O))|TransformedRest]) :-
    transform_nested_triples(Rest, S, OuterP, TransformedRest).
% Special version of triple for nested content
nested_triple_content([p(S,P,O)|More]) -->
   node(S),
   blanks,
   node(P),
   blanks,
   nested_objects(S, P, O, More).

% Special version of objects for nested triples
nested_objects(S, P, O, More) -->
   node(O),
   blanks,
   (
       ",",
       blanks,
       nested_objects(S, P, NextO, RestMore),
       { More = [p(S,P,NextO)|RestMore] }
   |
       ";",
       blanks,
       nested_predicate_objects(S, More)
   |
       { More = [] }
   ).

% Special version of predicate_objects for nested triples
nested_predicate_objects(S, [p(S,P,O)|More]) -->
   node(P),
   blanks,
   nested_objects(S, P, O, More).

% Handle remaining nested triples
remaining_nested_triples([]) --> [].
remaining_nested_triples(Triples) -->
   nested_triple_content(FirstTriples),
   blanks,
   ".",
   blanks,
   remaining_nested_triples(RestTriples),
   { append(FirstTriples, RestTriples, Triples) }.

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
   "(",
   blanks,
   graph_statements(Triples),
   blanks,
   ")".

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
     Id = '$VAR'(Name), !
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
   { \+ (code_type(C, alnum); C = 0'_ ; C = 0'-) }.

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
    test_nested_triples,
    test_many_nested_triples,
    % test_deeply_nested_triples,
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

test_nested_triples :-
    writeln('Testing nested triples syntax...'),
    parse_triples("a b { x1 y1 z1, z2; y2 z3. x2 y3 z4. }.", Triples),
    writeln(Triples).
test_many_nested_triples :-
    writeln('Testing many nested triples syntax...'),
    parse_triples("db delete { TaskId name OldName. }; insert { TaskId name NewName.}.", Triples),
    writeln(Triples).
test_deeply_nested_triples :-
    writeln('Testing deeply nested triples syntax...'),
    parse_triples("a b { x1 y1 z1, { m1 n1 p1; n2; p2, 3. m2 n3 p4. }; y2 z3. x2 y3 z4. }.", Triples),
    writeln(Triples).

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
        }.", Triples),
    writeln(Triples).

test_nested_graphs :-
    writeln('Testing deeply nested graphs...'),
    parse_triples("db hasGraph ( g1 contains ( \"user_1\" name \"bob\". ). g2 type \"group\". ).", Triples),
    writeln(Triples),
    
    writeln('Testing multiple nested graphs...'),
    parse_triples("db hasGraph ( g1 data ( a b c. ); metadata ( x y z. ). ).", Triples2),
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
    parse_triples("db hasGraph ( \"user_1\" name \"bob\". \"user_1\" age 20. ).", Triples),
    writeln(Triples).

test_nested_graph :-
    writeln('Testing nested graph...'),
    parse_triples("db hasGraph ( g1 contains ( \"user_1\" name \"bob\". ). ).", Triples),
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
    parse_triples("a -> b.", T1),
    writeln(T1),
    parse_triples("x <-> y.", T2),
    writeln(T2),
    parse_triples("p >= q.", T3),
    writeln(T3),
    parse_triples("m <= n.", T4),
    writeln(T4),
    parse_triples("x || y.", T5),
    writeln(T5),
    parse_triples("a && b.", T6),
    writeln(T6),
    parse_triples("f :- g.", T7),
    writeln(T7),
    parse_triples("x |> y.", T8),
    writeln(T8).
