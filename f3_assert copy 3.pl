:- [f3p].  % Include the parser file

% Make p/3 dynamic so we can assert to it
:- dynamic p/3.

% Builtins
builtin(>).
builtin(<).
builtin(>=).
builtin(<=).
builtin(sconcat).

b(A, >, B) :- A > B.
b(A, <, B) :- A  < B.
b(A, >=, B) :- A >= B.
b(A, =<, B) :- A =< B.
% String concatenation builtin
b(XS, sconcat, Res) :- 
    atomic_list_concat(XS, '', Res).

% Main predicate to process input
process_input(Filename) :-
    read_file_to_string(Filename, Input, []),
    parse_triples(Input, Triples),
    assert_all_triples(Triples),
    process_implications,
    write('Successfully asserted all triples.'), nl.

% Assert all triples from the parser
assert_all_triples([]).
assert_all_triples([p(S,P,O)|Rest]) :-
    assertz(p(S,P,O)),
    assert_all_triples(Rest).

% Process implications
process_implications :-
    findall(p(graph(G1), =>, graph(G2)), p(graph(G1), =>, graph(G2)), Rules),
    format('Found rules: ~w~n', [Rules]),
    maplist(assert_rule, Rules).
list_to_conjunction([], true).  % empty list becomes true
list_to_conjunction([Goal], Goal) :- !.  % single goal
list_to_conjunction([Goal|Goals], (Goal, Rest)) :-
    list_to_conjunction(Goals, Rest).

% Example usage:
build_rule(Head, ConditionsList, Rule) :-
    list_to_conjunction(ConditionsList, Body),
    Rule = (Head :- Body).

% Handle a single rule
assert_rule(p(graph(G1), =>, graph(G2))) :-
    format('Creating rule from G1: ~w to G2: ~w~n', [G1, G2]),
    % collect_vars(G1, VarMap),
    forall(member(M, G2), (
        %    (create_body(G1, Body, VarMap),
            % Map variables in the head using VarMap
            % map_vars(S2, VarMap, MappedS2),
            % map_vars(O2, VarMap, MappedO2),
            % Construct the head term
            % HeadTerm =.. [p, MappedS2, P2, MappedO2],
            % HeadTerm =.. [p, S2, P2, O2],
            build_rule(M, G1, Rule),
            format('Creating rule: ~w :- ~w~n', [M, Rule]),
            % Assert the full rule
            % assertz((HeadTerm :- Body)))).
            assertz(Rule))).


% Create the body of a rule with variable mapping
create_body([], true, _) :- !.
create_body([p(S,P,O)], Body, VarMap) :- !,
    (builtin(P) -> 
        BodyTerm =.. [b, S, P, O]
    ;   
        BodyTerm =.. [p, S, P, O]
    ),
    Body = BodyTerm.
create_body([p(S,P,O)|Rest], (Body,MoreConds), VarMap) :-
    create_body([p(S,P,O)], Body, VarMap),
    create_body(Rest, MoreConds, VarMap).

% Query that skips builtins and rules but allows derived facts
query(Pattern) :-
    findall(Pattern, 
            (p(S,P,O),
             \+ builtin(P),
             \+ P = '=>',  % Skip implications
             Pattern = p(S,P,O)),
            Facts),
    maplist(writeln, Facts).

% Main entry point
:- initialization(main).
main :-
    current_prolog_flag(argv, [Filename|_]),
    process_input(Filename).