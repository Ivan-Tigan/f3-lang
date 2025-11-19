:- module(sed, []).

:- use_module(library(plunit)).
:- use_module(library(debug)).

:- debug(log).

:- multifile user:p/3.

% Set test options to show output
:- set_test_options([silent(false)]).

:- use_module(runCLI).

% Main predicate: p(String, [sed, Pattern], Output)
user:p(String, [sed, Pattern], Output) :-
    % Build sed command string
    format(atom(SedCmd), 'sed "~w"', [Pattern]),
    
    % Use runCLI to execute sed with String as stdin
    % runCLI expects [CmdId, CmdString, StdIn]
    term_hash([sed, Pattern, String], CmdId),
    user:p([CmdId, SedCmd, String], runCLI, [Stdout, Stderr]),
    
    % Log any stderr and return stdout
    (   Stderr = "" ->
        true
    ;   format(user_error, 'sed stderr: ~w', [Stderr])
    ),
    
    Output = Stdout,
    format(user_error, 'sed input: ~w, pattern: ~w, output: ~w', [String, Pattern, Output]).

% Tests
:- begin_tests(sed).

test(substitute_simple) :-
    p("hello world", [sed, 's/world/universe/'], Result),
    sub_string(Result, _, _, _, "hello universe"), !.

test(substitute_global) :-
    p("foo bar foo", [sed, 's/foo/baz/g'], Result),
    sub_string(Result, _, _, _, "baz bar baz"), !.

test(delete_lines) :-
    p("line1\nline2\nline3\n", [sed, '/line2/d'], Result),
    \+ sub_string(Result, _, _, _, "line2"),
    sub_string(Result, _, _, _, "line1"),
    sub_string(Result, _, _, _, "line3"), !.

:- end_tests(sed).
:- set_prolog_flag(plunit_output, always).

% Run tests and halt
% :- (run_tests -> halt(0) ; halt(1)).