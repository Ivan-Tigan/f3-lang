:- module(runCLI, []).

:- use_module(library(process)).
:- use_module(library(plunit)).
:- use_module(library(debug)).

:- debug(log).

:- multifile user:b/3.
:- multifile user:builtin/1.

% Set test options to show output
:- set_test_options([silent(false)]).

:- use_module(cache).

user:builtin(runCLI).
% Main predicate to handle command execution with caching
user:b(Input, runCLI, Output) :-
    access_cache(Input, runCLI, Output), !.
user:b([CmdId, CmdString, StdIn], runCLI, Output) :-
    % Execute command through bash and capture output
    setup_call_cleanup(
        process_create(path(bash), ['-c', CmdString], 
            [stdin(pipe(In)), stdout(pipe(Out)), stderr(pipe(Err))]),
        (   write(In, StdIn),
            close(In),
            read_string(Out, _, Stdout),
            read_string(Err, _, Stderr)
        ),
        (   close(Out),
            close(Err))
    ),
    
    % Construct response as [stdout, stderr]
    Output = [Stdout, Stderr],
    debug(log, 'input ~n~q~noutput ~n~q', [CmdString, Output]),
 
    % Write to cache and cut to prevent backtracking
    write_cache([CmdId, CmdString, StdIn], runCLI, Output), !.

% Tests
:- begin_tests(run_cli).
test(echo_command) :-
    b([1, "echo 'Hello World'", ""], runCLI, [Stdout, ""]),
    sub_string(Stdout, _, _, _, "Hello World"), !.

test(cat_with_stdin) :-
    b([2, "cat", "test input"], runCLI, ["test input", ""]).

test(grep_with_stdin) :-
    b([3, "grep hello", "hello world\nhi there\nhello again\n"], runCLI, 
      ["hello world\nhello again\n", ""]).

:- end_tests(run_cli).
:- set_prolog_flag(plunit_output, always).

% Run tests and halt
% :- (run_tests -> halt(0) ; halt(1)).