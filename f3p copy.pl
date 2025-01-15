:- debug(parser).
:- dynamic parse_vars/1.
:- dynamic p/3.
:- multifile p/3.
:- use_module(library(dcg_util)).
:- use_module(library(lambda)).
:- use_module(library(apply)).

:- dynamic debug_mode/1.
:- initialization(assertz(debug_mode(false))).

debug_print(Format, Args) :- (debug_mode(true) -> format(user_error, Format, Args) ; true).
enable_debug :- retractall(debug_mode(_)), assertz(debug_mode(true)).
disable_debug :- retractall(debug_mode(_)), assertz(debug_mode(false)).


f3pProgram(Triples) --> ws, list(spo, ws, ManyTriples), ws , {append(ManyTriples, Triples)}.

node(Node) -->
   ( quoted_string(Node), ! | number(Node), ! | symbol(Node), ! | identifier(Node), ! | list_node(Node), ! | nested_graph(Node), !),
   {debug_print("Parsed node: ~q~n", [Node])} .

semicolon --> ws, ";", ws.
comma --> ws, ",", ws.

spo(Triples) --> node(S), {debug_print("spo start S: ~q~n", [S])}, ws1, pos(POs), {debug_print("spo mid POs: ~q~n", [POs])}, {maplist(S+\X^Y^(X=[P,O], Y=p(S,P,O)), POs, Triples)}, {debug_print("spo end Triples: ~q~n", [Triples])}, ".".
pos(POs) --> list(po, semicolon, NestedPOs), {debug_print("pos mid NestedPOs: ~q~n", [NestedPOs])}, {append(NestedPOs, POs) }.
po(PO) --> node(P), {debug_print("po start P: ~q~n", [P])}, ws1, os(Os), {debug_print("po mid Os: ~q~n", [Os])}, {maplist(P+\X^Y^(X=O, Y=[P,O]), Os, PO)}.
os(Os) --> {debug_print("os start: ~n", [])}, list(o, comma, NestedOs), {debug_print("os mid NestedOs: ~q~n", [NestedOs])}, {append(NestedOs, Os) }.
o(O) --> node(N), !, {debug_print("o start N: ~q~n", [N])}, {O = [N]}, {debug_print("o end O: ~q~n", [O])}.
o(O) --> "{", !, ws, list(spo, ws, ManyTriples), ws, "}", {append(ManyTriples, O)}.


nested_graph(graph(Triples)) --> "(",!, ws, list(spo, ws, ManyTriples), ws, ")", {append(ManyTriples, Triples)}.

list_node(Items) --> "[", ws, list(node, ws1, Items), ws, "]".
symbol(X) --> at_least(1, symbol_char, CS), { atom_codes(X, CS) }.
symbol_char(C) --> [C], {member(C, [ 0'-, 0'>, 0'<, 0'=, 0'+, 0'*, 0'/, 0'\\, 0'^, 0'~, 0'#, 0'$, 0'@, 0'?, 0'|, 0':, 0'&, 0'!, 0';, 0'+ ])}.

identifier(Id) --> 
   [C],
   { char_code(Char, C),  % Convert code to character
     debug_print("Trying to parse identifier starting with: '~w' (code: ~w)~n", [Char, C]) },
   { code_type(C, alpha) },
   { debug_print("Confirmed first character is alphabetic~n", []) },
   at_least(0, id_rest, Cs),
   { maplist(char_code, RestChars, Cs),  % Convert all codes to characters
     atomic_list_concat(RestChars, '', RestString),
     debug_print("Rest of identifier: '~w'~n", [RestString]) },
   { atom_codes(Name, [C|Cs]),
     debug_print("Full identifier: '~w'~n", [Name]),
     (char_type(C, upper) -> Id = '$VAR'(Name), !  ; Id = Name)
   }.
id_rest(C) --> [C], { code_type(C, alnum); C = 0'_; C = 0'- }.

number(N) --> at_least(1, digit, Ds), { number_codes(N, Ds) }.
digit(D) --> [D], { code_type(D, digit) }.

quoted_string(String) --> "\"", !, at_least(0, string_char, Chars), "\"", { string_chars(String, Chars) }, { debug_print("Quoted string: ~q~n", [String]) } .
 
string_char(C) --> "\\", "\"", !,  { C = 34 }.
string_char(C) --> [C], { C \= 34 }.

ws1 --> at_least(1, pspace).
ws --> greedy(pspace).

pspace(C) --> [C], { code_type(C, space) }.

parse_triples(Input, Triples) :- 
    string_codes(Input, Codes),
    (phrase(f3pProgram(Triples), Codes) ->
        true
    ;   
    debug_print("Parse error: ~w~n", [Input]),
        % Get position of error by counting consumed codes
        length(Codes, Total),
        phrase(f3pProgram(Triples), Codes, Rest),
        length(Rest, Remaining),
        Position is Total - Remaining,
        debug_print('Parse error near position ~w~n', [Position]),
        fail
    ).
% Tests
test_all :-
    writeln('Running all tests...'),
    test_nested_triples,
    test_many_nested_triples,
    test_deeply_nested_triples,
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

test_nested_graph :-
    writeln('Testing nested graph...'),
    parse_triples("db hasGraph ( g1 contains ( \"user_1\" name \"bob\". ). ).", Triples),
    Expected = [p(db,hasGraph,graph([p(g1,contains,graph([p("user_1",name,"bob")]))]))],
    assertion(Triples = Expected).
test_nested_triples :-
    writeln('Testing nested triples syntax...'),
    parse_triples("a b { x1 y1 z1, z2; y2 z3. x2 y3 z4. }.", Triples),
    Expected = [p(a,b,p(x1,y1,z1)), p(a,b,p(x1,y1,z2)), p(a,b,p(x1,y2,z3)), p(a,b,p(x2,y3,z4))],
    assertion(Triples = Expected).

test_many_nested_triples :-
    writeln('Testing many nested triples syntax...'),
    parse_triples("db delete { TaskId name OldName. }; insert { TaskId name NewName. }.", Triples),
    Expected = [p(db,delete,p(TaskId,name,OldName)), p(db,insert,p(TaskId,name,NewName))],
    assertion(Triples = Expected).

test_deeply_nested_triples :-
    writeln('Testing deeply nested triples syntax...'),
    parse_triples("a b { x1 y1 z1, { m1 n1 p1; n2 p2, 3. m2 n3 p4. }; y2 z3. x2 y3 z4. }.", Triples),
    Expected = [
        p(a,b,p(x1,y1,z1)),
        p(a,b,p(x1,y1,p(m1,n1,p1))),
        p(a,b,p(x1,y1,p(m1,n2,p2))),
        p(a,b,p(x1,y1,p(m1,n2,3))),
        p(a,b,p(x1,y1,p(m2,n3,p4))),
        p(a,b,p(x1,y2,z3)),
        p(a,b,p(x2,y3,z4))
    ],
    assertion(Triples = Expected).

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
    Expected = [
        p(html,attribute,class),
        p(html,attribute,href),
        p(html,attribute,id),
        p(html,attribute,l1),
        p(html,attribute,l2),
        p(html,attribute,l3),
        p(html,element,a),
        p(html,element,div),
        p(homePage,a,page),
        p(homePage,path,""),
        p(homePage,content,homePageContent),
        p(homePageContent,a,div),
        p(homePageContent,class,"flex w-full h-full justify-center"),
        p(homePageContent,child,[homePageContent,inner]),
        p([homePageContent,inner],a,div),
        p([homePageContent,inner],class,"flex flex-col justify-start p-2"),
        p([db,"world"],hasGraph,p("user_1",a,user)),
        p([db,"world"],hasGraph,p("user_1",name,"Bob")),
        p([db,"world"],hasGraph,p("user_1",hasTask,"user_1/task_1")),
        p([db,"world"],hasGraph,p("user_1",hasTask,"user_1/task_2")),
        p([db,"world"],hasGraph,p("user_1",hasTask,"user_1/task_3")),
        p([db,"world"],hasGraph,p("user_1/task_1",name,"Bob's first task")),
        p([db,"world"],hasGraph,p("user_1/task_2",name,"Bob's second task")),
        p([db,"world"],hasGraph,p("user_1/task_3",name,"Bob's third task"))
    ],
    assertion(Triples = Expected).

test_basic_triple :-
    writeln('Testing basic triple...'),
    parse_triples("bob name \"Robert\".", Triples),
    Expected = [p(bob,name,"Robert")],
    assertion(Triples = Expected).

test_multiple_objects :-
    writeln('Testing multiple objects...'),
    parse_triples("bob likes alice, carol, david.", Triples),
    Expected = [p(bob,likes,alice), p(bob,likes,carol), p(bob,likes,david)],
    assertion(Triples = Expected).

test_multiple_predicates :-
    writeln('Testing multiple predicates...'),
    parse_triples("bob likes alice; hates \"carol\"; knows david.", Triples),
    Expected = [p(bob,likes,alice), p(bob,hates,"carol"), p(bob,knows,david)],
    assertion(Triples = Expected).

test_graph :-
    writeln('Testing graph...'),
    parse_triples("db hasGraph ( \"user_1\" name \"bob\". \"user_1\" age 20. ).", Triples),
    Expected = [p(db,hasGraph,graph([p("user_1",name,"bob"), p("user_1",age,20)]))],
    assertion(Triples = Expected).

test_nested_graphs :-
    writeln('Testing deeply nested graphs...'),
    parse_triples("db hasGraph ( g1 contains ( \"user_1\" name \"bob\". ). g2 type \"group\". ).", Triples),
    Expected1 = [p(db,hasGraph,graph([
        p(g1,contains,graph([p("user_1",name,"bob")])),
        p(g2,type,"group")
    ]))],
    assertion(Triples = Expected1),
    
    writeln('Testing multiple nested graphs...'),
    parse_triples("db hasGraph ( g1 data ( a b c. ); metadata ( x y z. ). ).", Triples2),
    Expected2 = [p(db,hasGraph,graph([
        p(g1,data,graph([p(a,b,c)])),
        p(g1,metadata,graph([p(x,y,z)]))
    ]))],
    assertion(Triples2 = Expected2).

test_mixed_nodes :-
    writeln('Testing mixed node types...'),
    parse_triples("123 hasString \"test\". \"test\" hasNumber 456.", Triples),
    Expected = [p(123,hasString,"test"), p("test",hasNumber,456)],
    assertion(Triples = Expected).

test_list_nodes :-
    writeln('Testing list nodes...'),
    parse_triples("bob friends [alice carol 123 \"david\"].", Triples),
    Expected = [p(bob,friends,[alice,carol,123,"david"])],
    assertion(Triples = Expected).

test_symbols :-
    writeln('Testing symbols...'),
    parse_triples("a -> b.", T1),
    assertion(T1 = [p(a,'->',b)]),
    
    parse_triples("x <-> y.", T2),
    assertion(T2 = [p(x,'<->',y)]),
    
    parse_triples("p >= q.", T3),
    assertion(T3 = [p(p,'>=',q)]),
    
    parse_triples("m <= n.", T4),
    assertion(T4 = [p(m,'<=',n)]),
    
    parse_triples("x || y.", T5),
    assertion(T5 = [p(x,'||',y)]),
    
    parse_triples("a && b.", T6),
    assertion(T6 = [p(a,'&&',b)]),
    
    parse_triples("f :- g.", T7),
    assertion(T7 = [p(f,':-',g)]),
    
    parse_triples("x |> y.", T8),
    assertion(T8 = [p(x,'|>',y)]).

test_escaped_quotes :-
    writeln('Testing escaped quotes...'),
    parse_triples("bob \"ws-connected\" \"send:{\\\"message\\\": \\\"Hello, I just connected!\\\"}\". ", Triples),
    Expected = [p(bob,"ws-connected","send:{\"message\": \"Hello, I just connected!\"}")],
    assertion(Triples = Expected).

