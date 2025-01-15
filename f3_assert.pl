% :- module(f3_assert, [p/3, process_input/1]).
:- use_module(library(crypto)).
:- use_module(library(pcre)).

% :- [f3p].  % Include the parser file
:- dynamic loaded/1.
:- multifile loaded/1.

% Make p/3 dynamic so we can assert to it
:- dynamic p/3.
:- multifile p/3.

load(DB) :-
    (loaded(DB) -> 
        true ;
        (exists_file(DB) -> consult(DB) ; true),
        assertz(loaded(DB))
    ).


% Builtins
builtin(=).
builtin(>).
builtin(<).
builtin(>=).
builtin(<=).
builtin(sconcat).
builtin(collect).
builtin(query).
builtin(sha256). 
builtin([Index]) :- number(Index), !.
builtin(hasGraph).
builtin([X, >>, Y]).
builtin(replaceQuotes).
builtin(lconcat).
builtin(not).

b(A, =, B) :- A = B.
b(A, >, B) :- A > B.
b(A, <, B) :- A < B.
b(A, >=, B) :- A >= B.
b(A, =<, B) :- A =< B.

b(system, not, p(A,B,C)) :- \+ p(A,B,C).
% String concatenation builtin
b(XS, sconcat, Res) :- 
   atomic_list_concat(XS, '', A),
   atom_string(A, Res).

b([Xs, Ys], lconcat, Res) :- 
   append(Xs, Ys, Res).
b(Xs, [Index], Element) :- 
    number(Index),
    !,
    nth1(Index, Xs, Element).
b(X, replaceQuotes, Y) :-
   replace_substring(X, "'", "\"", Res).
   
b([Path, graph(G)], collect, Results) :-
    % Convert graph pattern to conjunction
    list_to_conjunction(G, Conjunction),
    % Collect first argument from each triple that matches the pattern
    findall(Path, Conjunction, Results).
b(A, [X, >>, Y], B) :- builtin(X), builtin(Y), !, b(A, X, C), b(C, Y, B) .
b(A, [X, >>, Y], B) :- builtin(X), !, b(A, X, C), p(C, Y, B) .
b(A, [X, >>, Y], B) :- builtin(Y), !, p(A, X, C), b(C, Y, B) .
b(A, [X, >>, Y], B) :- p(A, X, C), p(C, Y, B) .

b(graph(G1s), query, graph(G2s)) :-
   % writeln('Query:'),
   % writeln(G2s),
   % writeln('Against:'),
   % writeln(G1s),
   query_match(G2s, G1s).
% b(Input, sha256, Hash) :-
%     atomic(Input),
%     crypto_data_hash(Input, Hash, [algorithm(sha256), encoding(hex)]).
b(Input, sha256, Hash) :-
    atomic(Input),
    term_string(Input, InputString),  % Convert input to string
    crypto_data_hash(InputString, Hash, [algorithm(sha256), encoding(octet)]).
b([db, DBPath], hasGraph, P) :-
   full_db_path(DBPath, FullPath),
    load(FullPath),
    p([db, DBPath], hasGraph, P).
replace_substring(String, To_Replace, Replace_With, Result) :-
    (    append([Front, To_Replace, Back], String)
    ->   append([Front, Replace_With, Back], R),
         replace_substring(Back, To_Replace, Replace_With, Result)
    ;    Result = String
    ).


query_match([], _).
query_match([p(S,P,O)|Rest], Source) :-
   % writeln('Matching:'),
   % writeln(p(S,P,O)),
   member(p(S,P,O), Source),
   query_match(Rest, Source).
full_db_path(DBPath, FullPath) :-
    atom_concat('db/', DBPath, FullPath).
insert_db_fact(p([db, DBPath], hasGraph, P)) :-
    full_db_path(DBPath, FullPath),
    load(FullPath),
    F = p([db, DBPath], hasGraph, P),
    (p([db, DBPath], hasGraph, P) -> 
        true
    ;   
        assertz(F),
        file_directory_name(FullPath, Dir),
        % Create directory if it doesn't exist
        (exists_directory(Dir) -> 
            true 
        ;   
            make_directory(Dir)
        ),

        open(FullPath, append, Stream),
        format(Stream, '~q.~n', [F]),
        close(Stream)
    ).
delete_db_fact(p([db, DBPath], hasGraph, P)) :-
    full_db_path(DBPath, FullPath),
    F = p([db, DBPath], hasGraph, P),
    % Create sed pattern by converting fact to quoted string
    format(string(Pattern), "~q", [F]),
    % Use single quotes around the pattern and escape single quotes in pattern
    atomic_list_concat(['sed -i \'/^', Pattern, '\\.$/d\' ', FullPath], CommandUnescaped),
    % Escape square brackets
    split_string(CommandUnescaped, "[", "", Parts1),
    atomic_list_concat(Parts1, "\\[", Temp1),
    split_string(Temp1, "]", "", Parts2),
    atomic_list_concat(Parts2, "\\]", Command),
    
    format(user_error, "~n Command: ~w~n", [Command]),
    shell(Command),
    format(user_error, "sed completed~n", []),
    % Remove from memory
    retract(F),
    format(user_error, "fact deleted~n", []).
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
  % Then write rules
   forall(member(Rule, Rules),
          process_rule(Rule, Stream)),
   close(Stream),
   % Read and print the file contents
   read_file_to_string(File, Contents, []),
   format("Generated facts and rules:~n~w~n", [Contents]),
   % load_files(File, [dynamic(true)]).  % Use load_files with dynamic option

   consult(File)
   .

% Process single rule to stream
process_rule(p(graph(G1), =>, graph(G2)), Stream) :-
   format('Creating rule from G1: ~w to G2: ~w~n', [G1, G2]),
   forall(member(M, G2), (
       build_rule(M, G1, Rule),
       format('Creating rule: ~w~n', [Rule]),
      %  assertz(Rule)
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

triple_string(p(A,B,C), S) :- node_string(A, As), node_string(B, Bs), node_string(C, Cs), atomic_list_concat([As, Bs, Cs], " ", ST), atomic_list_concat([ST, '.'] , "", S), !.
  

node_string(N, S) :- string(N), atomic_list_concat(['"', N, '"'], '', S),!.
node_string(N,S) :- atom(N), atom_string(N, S), !.
node_string(N, S) :- number(N), number_string(N, S), !.
node_string(Triple, S) :- triple_string(Triple, TS), atomic_list_concat(['{', ' ', TS, ' ', '}'], "", S), !.
node_string(graph(Triples), S) :- maplist(triple_string, Triples, Ss), atomic_list_concat(Ss, '\n', SG), atomic_list_concat(['(', SG, ')'], ' ', S), !.
node_string(Ns, S) :- atomic_list_concat(Ns, ' ', C), atom_string(C, S), !.
print_node(T) :- node_string(T, S),!, writeln(S).
print_node(T) :- write_canonical(T), nl.

% Main entry point
% :- initialization(main).
:-style_check(-singleton).
main :-
   % consult(user_input),
   [user],
     tmp_file_stream(text, File, Stream),
   % format("Created temporary file: ~w~n", [File]),
   write(Stream, ':- dynamic p/3.\n\n'),

   forall(p(graph(G1), => ,graph(G2)), (
         forall(member(M, G2), (
               build_rule(M, G1, Rule),
               % format('Creating rule: ~w~n', [Rule]),
               %  assertz(Rule)
               format(Stream, '~q.~n', [Rule])
            ))
      )),
   close(Stream),
   % Read and print the file contents
   read_file_to_string(File, Contents, []),
   % format("Generated facts and rules:~n~w~n", [Contents]),
   % load_files(File, [dynamic(true)]).  % Use load_files with dynamic option

   consult(File),
   % listing(p),
   % format(user_error, "~n Results 1 :~n", []),
   forall((p(system, query, [Path, graph(G)])), (
          list_to_conjunction(G, Conjunction),
    % Collect first argument from each triple that matches the pattern
      findall(Path, Conjunction, Results),
      maplist(print_node, Results)
      
      % writeln(Results)
      ))

   .
   % current_prolog_flag(argv, [Filename|_]),
   % process_input(Filename).