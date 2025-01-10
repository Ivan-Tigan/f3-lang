% :- module(f3_assert, [p/3, process_input/1]).
:- use_module(library(crypto)).

:- [f3p].  % Include the parser file

% Make p/3 dynamic so we can assert to it
:- dynamic p/3.
:- multifile p/3.
% Builtins
builtin(>).
builtin(<).
builtin(>=).
builtin(<=).
builtin(sconcat).
builtin(collect).
builtin(query).
builtin(sha256). 

b(A, >, B) :- A > B.
b(A, <, B) :- A < B.
b(A, >=, B) :- A >= B.
b(A, =<, B) :- A =< B.
% String concatenation builtin
b(XS, sconcat, Res) :- 
   atomic_list_concat(XS, '', A),
   atom_string(A, Res).
b([Path, graph(G)], collect, Results) :-
    % Convert graph pattern to conjunction
    list_to_conjunction(G, Conjunction),
    % Collect first argument from each triple that matches the pattern
    findall(Path, Conjunction, Results).


b(graph(G1s), query, graph(G2s)) :-
   % writeln('Query:'),
   % writeln(G2s),
   % writeln('Against:'),
   % writeln(G1s),
   query_match(G2s, G1s).
b(Input, sha256, Hash) :-
    atomic(Input),
    crypto_data_hash(Input, Hash, [algorithm(sha256), encoding(hex)]).

query_match([], _).
query_match([p(S,P,O)|Rest], Source) :-
   % writeln('Matching:'),
   % writeln(p(S,P,O)),
   member(p(S,P,O), Source),
   query_match(Rest, Source).
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
   % Get both facts and rules
   findall(p(S,P,O), (p(S,P,O), \+ P = '=>'), Facts),  % Get all non-rule facts
   findall(p(graph(G1), =>, graph(G2)), p(graph(G1), =>, graph(G2)), Rules),
   format('Found rules: ~w~n', [Rules]),
   write_facts_and_rules(Facts, Rules).

write_facts_and_rules(Facts, Rules) :-
   tmp_file_stream(text, File, Stream),
   format("Created temporary file: ~w~n", [File]),
   write(Stream, ':- dynamic p/3.\n\n'),
   % Write facts first
   forall(member(Fact, Facts),
          format(Stream, '~q.~n', [Fact])),
   % Then write rules
   forall(member(Rule, Rules),
          process_rule(Rule, Stream)),
   close(Stream),
   % Read and print the file contents
   read_file_to_string(File, Contents, []),
   format("Generated facts and rules:~n~w~n", [Contents]),
   load_files(File, [dynamic(true)]).  % Use load_files with dynamic option

   consult(File)
   .

% Process single rule to stream
process_rule(p(graph(G1), =>, graph(G2)), Stream) :-
   format('Creating rule from G1: ~w to G2: ~w~n', [G1, G2]),
   forall(member(M, G2), (
       build_rule(M, G1, Rule),
       format('Creating rule: ~w~n', [Rule]),
       format(Stream, '~q.~n', [Rule])
   )).

% Convert a single condition to the right form (b/3 or p/3)
transform_condition(p(S,P,O), b(S,P,O)) :- 
   ground(P),  % Only check for builtins if P is ground
   builtin(P),
   !.
transform_condition(p(S,P,O), p(S,P,O)).

% Convert list of conditions to conjunction, transforming builtins
list_to_conjunction([], true).
list_to_conjunction([Cond], TransformedCond) :- !,
   transform_condition(Cond, TransformedCond).
list_to_conjunction([Cond|Conds], (TransformedCond, Rest)) :-
   transform_condition(Cond, TransformedCond),
   list_to_conjunction(Conds, Rest).

% Build rule with transformed conditions 
build_rule(Head, ConditionsList, Rule) :-
   list_to_conjunction(ConditionsList, Body),
   Rule = (Head :- Body).

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
% :- initialization(main).
% main :-
%    current_prolog_flag(argv, [Filename|_]),
%    process_input(Filename).