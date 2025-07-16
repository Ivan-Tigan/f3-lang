:- consult("server.pl").

% :- module(f3_assert, [p/3, process_input/1]).
:- use_module(library(crypto)).
:- use_module(library(pcre)).
:- use_module(builtins/cache).
:- use_module(builtins/http).
:- use_module(builtins/match).
:- use_module(builtins/lmdb).
:- use_module(builtins/base64).
:- use_module(builtins/json).
:- use_module(builtins/crypto).
:- use_module(builtins/pipe).
:- use_module(builtins/clpfd).
:- use_module(builtins/math).
:- use_module(builtins/clpqr).

% :- [f3p].  % Include the parser file
:- dynamic loaded/1.
:- multifile loaded/1.

% Make p/3 dynamic so we can assert to it
% :- table p/3.
:- dynamic p/3.
:- multifile p/3.

:- dynamic f3_loaded/1.


load(DB) :-
    (loaded(DB) -> 
        true ;
        (exists_file(DB) -> consult(DB) ; true),
        assertz(loaded(DB))
    ).



p(system, now, T) :- get_time(T).

% Time parsing predicates
p(TimeStampString, parseTimeStamp, TimeStamp) :- parse_time(TimeStampString, TimeStamp).
p(TimeString, parseTime, Seconds) :- 
    % Parse HH:MM format to seconds since midnight
    atomic_list_concat([HourAtom, MinuteAtom], ':', TimeString),
    atom_number(HourAtom, Hour),
    atom_number(MinuteAtom, Minute),
    Seconds is Hour * 3600 + Minute * 60.
p(TimeStamp, [timeStampToString, Format], FormattedString) :- 
    format_time(string(FormattedString), Format, TimeStamp).


p(A, =, B) :- A = B.
p(A, neq, B) :- p(system, log, ["QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ", test, A, not, equal, to, B]), A \= B.
p(A, >, B) :- A > B.
p(A, <, B) :- A < B.
p(A, >=, B) :- A >= B.
p(A, =<, B) :- A =< B.
p(system, log, X) :- node_string(X, S),!, format(user_error, "~w~n", [S]).
p(system, log, X) :- format(user_error, "~q~n", [X]), !.
p(X, toString, S) :- node_string(X, S).
p(S, parseInteger, I) :- 
    atom_string(S, Str),
    atom_number(Str, I).
% Iter builtin to match each element of a list
p(List, iter, Element) :- 
    is_list(List),
    member(Element, List).

p(List, length, Length) :- 
    is_list(List),
    length(List, Length).

p(system, not, p(A,B,C)) :- \+ p(A,B,C).
p(XS, sconcat, Res) :- atomics_to_string(XS, Res) .
p(S, [splitString, Separator], Res) :- 
   atom_string(S, SStr),
   format(user_error, "ZZZZZZZZZZZZZZZZZZZZ Splitting string: ~q with separator: ~q~n", [SStr, Separator]),
    split_string(SStr, Separator, "", Res),
    
   format(user_error, "ZZZZZZZZZZZZZZZZZZZZ split success: ~q~n", [Res])
    .  

p(X, [reverse, Pred], Y) :- 

   !,
   p(Y, Pred, X).  
p(X, [reverse, Pred], Y) :-
   p(Y, Pred, X), !.  
   
p([Xs, Ys], lconcat, Res) :- 
   append(Xs, Ys, Res).
p(Xs, [Index], Element) :- 
    number(Index),
    !,
    nth0(Index, Xs, Element).
p(X, replaceQuotes, Y) :-
   replace_substring(X, "'", "\"", Res).
   
p([Path, graph(G)], collect, Results) :-
    % Convert graph pattern to conjunction
    list_to_conjunction(G, Conjunction),
    % Collect first argument from each triple that matches the pattern
    findall(Path, Conjunction, Results).




p(Input, sha256, Hash) :-
    atomic(Input),
    term_string(Input, InputString),  % Convert input to string
    crypto_data_hash(InputString, Hash, [algorithm(sha256), encoding(octet)]).
p([db, DBPath], hasGraph, P) :-
   full_db_path(DBPath, FullPath),
    load(FullPath),
    p([db, DBPath], hasGraph, P).
replace_substring(String, To_Replace, Replace_With, Result) :-
    (    append([Front, To_Replace, Back], String)
    ->   append([Front, Replace_With, Back], R),
         replace_substring(Back, To_Replace, Replace_With, Result)
    ;    Result = String
    ).



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
   % write(Stream, ':- table p/3.\n\n'),
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

transform_condition(p(system,cut,[]), !) :- !. 
transform_condition(p(system,fail,[]), fail) :- !. 

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

            \+ P = '=>',  % Skip implications
            Pattern = p(S,P,O)),
           Facts),
   maplist(writeln, Facts).

triple_string(p(A,B,C), S) :- node_string(A, As), node_string(B, Bs), node_string(C, Cs), atomic_list_concat([As, Bs, Cs], " ", ST), atomic_list_concat([ST, '.'] , "", S), !.
  

node_string(N, S) :- string(N), atomic_list_concat(['"', N, '"'], '', S),!.
node_string(N,S) :- atom(N), atom_string(N, S), !.
node_string(N, S) :- number(N), number_string(N, S), !.
node_string(N, S) :- var(N), atom_string('_UNBOUND_', S), !.
node_string(Triple, S) :- triple_string(Triple, TS), atomic_list_concat(['{', ' ', TS, ' ', '}'], "", S), !.
% node_string(graph(Triples), S) :- maplist(triple_string, Triples, Ss), atomic_list_concat(Ss, '\n', SG), atomic_list_concat(['(', SG, ')'], ' ', S), !.
node_string(graph([]), S) :-
    atomic_list_concat(['(\n\n)'], '', S), !.

node_string(graph([Triple]), S) :- 
    % Single triple without nested graphs
    triple_string(Triple, TS),
    \+ sub_term(graph(_), Triple),
    atomic_list_concat(['(', TS, ')'], ' ', S), !.

node_string(graph(Triples), S) :- 
    maplist(triple_string, Triples, RawSs),
    maplist(add_double_indent, RawSs, Ss),
    atomic_list_concat(Ss, '\n', SG),
    atomic_list_concat(['(\n', SG, '\n)'], '', S), !.


node_string(XS, S) :- 
    is_list(XS),
    maplist(node_string, XS, SS),
    atomic_list_concat(SS, ' ', Inner),
    atomic_list_concat(['[', Inner, ']'], ' ', S), !.
% node_string(Ns, S) :- term_string(Ns, S).
%  maplist(node_string, Ns, Cs), atomic_list_concat(CS, ' ', C), atom_string(C, S), !.
node_string(X, S) :- term_string(X, S).

add_double_indent(Line, Indented) :-
    atomic_list_concat(['  ', Line], '', Indented).


% Helper predicate to add indentation
add_indent(Line, Indented) :-
    atomic_list_concat(['  ', Line], '', Indented).

print_node(T) :- node_string(T, S),!, writeln(S).
print_node(T) :- write_canonical(T), nl.


run_command(Command, Output) :-
    process_create(path(sh), ['-c', Command],
        [stdout(pipe(Out))]),
    read_string(Out, _, Output),
    close(Out).

consult_string(S) :-
   tmp_file_stream(text, File, Stream),
   write(Stream, S),
   close(Stream),
   consult(File).
consult_f3_file(F) :-
   consult_f3_file_with_base(F, '').

consult_f3_file_with_base(F, BaseDir) :-
   % Resolve file path relative to base directory
   (BaseDir = '' ->
      ResolvedFile = F
   ;  % Handle relative paths properly  
      (sub_atom(F, 0, 2, _, './') ->
         % Remove ./ prefix and resolve relative to BaseDir
         sub_atom(F, 2, _, 0, RelativePath),
         atom_concat(BaseDir, '/', BaseDirSlash),
         atom_concat(BaseDirSlash, RelativePath, ResolvedFile)
      ;  % Regular relative path
         atom_concat(BaseDir, '/', BaseDirSlash),
         atom_concat(BaseDirSlash, F, ResolvedFile)
      )
   ),
   
   % Get absolute path to avoid duplicates
   absolute_file_name(ResolvedFile, AbsoluteFile),
   
   (f3_loaded(AbsoluteFile) -> true; 

   % Try F3_HOME env first, then SNAP, then fallback to local
   (getenv('F3_HOME', Home) -> 
      p([Home, "/f3p ", ResolvedFile], sconcat, Cmd)
   ; getenv('SNAP', Snap) -> 
      p([Snap, "/bin/f3p ", ResolvedFile], sconcat, Cmd)
   ;   
      p(["./f3p ", ResolvedFile], sconcat, Cmd)
   ),
   run_command(Cmd, Program),
   consult_string(Program),
   assertz(f3_loaded(AbsoluteFile)),

   % Get directory of current file for relative includes
   file_directory_name(AbsoluteFile, CurrentDir),
   
   forall((p(system, include, E)), (
      consult_f3_file_with_base(E, CurrentDir)
   ))
).

% Main entry point
% :- initialization(main).
:-style_check(-singleton).
main([Arg1, Arg2]) :-
   % consult(user_input),
   % [user],
   consult_f3_file(Arg2),
   tmp_file_stream(text, File, Stream),
   % format("Created temporary file: ~w~n", [File]),
   write(Stream, ':- dynamic p/3.\n\n'),
   % listing(p),
   forall(clause(p(A, B, C), true),
      (retract(p(A, B, C)),  % Remove from memory to avoid duplicates
      (p(A,B,C) = p(graph(G1), => ,graph(G2)) -> 
         forall(member(M, G2), (
               build_rule(M, G1, Rule),
               % format('Creating rule: ~w~n', [Rule]),
               %  assertz(Rule)
               format(Stream, '~q.~n', [Rule])
            ))
      ; format(Stream, '~q.~n', [p(A,B,C)])
      ))),
   close(Stream),
   % retractall(p(_,_,_)),  % Clear all p/3 facts to avoid duplicates
   % Read and print the file contents
   read_file_to_string(File, Contents, []),
   % format("Generated facts and rules:~n~w~n", [Contents]),
   % load_files(File, [dynamic(true)]).  % Use load_files with dynamic option

   consult(File),

   tmp_file_stream(text, File2, Stream2),
   forall((p(system, rawProlog, E)), (
      format(user_error, "Raw prolog: ~w~n", [E]),
      format(Stream2, '~w~n', [E])      
      )),
   close(Stream2),
   read_file_to_string(File2, Contents2, []),
   % format("Generated raw prolog:~n~w~n", [Contents2]),
   consult(File2),

   forall((p(system, staticError, E)), (
      ansi_format([fg(red)],"Static error: ~w~n", [E])
      )),
   (p(system, staticError, _) -> halt(1); true),
   % listing(p),
   % format(user_error, "~n Results 1 :~n", []),
   (Arg1 = run ->
   forall((p(system, query, [Path, graph(G)])), (
          list_to_conjunction(G, Conjunction),
    % Collect first argument from each triple that matches the pattern
      findall(Path, Conjunction, Results),
      maplist(print_node, Results)
      
      % writeln(Results)
      )),
   forall(p(system, debug, listing), 
      listing(p)
      ),
      (p(system, runWebServer, PORT) -> (writeln("Start server"), start_server(PORT)); true) 
   ; true
   )
   .
   % current_prolog_flag(argv, [Filename|_]),
   % process_input(Filename).
