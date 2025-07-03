:- module(pipe_builtin, []).

:- use_module(library(plunit)).

:- multifile user:b/3.
:- multifile user:builtin/1.

% Set test options to show output
:- set_test_options([silent(false)]).

% Traditional two-element pipe [A >> B]
user:builtin([X, >>, Y]).

% Multi-element pipe chains [A >> B >> C >> D >> ...]
user:builtin(Chain) :- 
    is_list(Chain),
    length(Chain, Len),
    Len >= 5,  % At least [A >> B >> C]
    Len mod 2 =:= 1,  % Odd length: A >> B >> C
    is_pipe_chain(Chain).

% Check if a list is a valid pipe chain (alternating elements and >>)
is_pipe_chain([_]).  % Single element is valid
is_pipe_chain([_, >>, Rest|Tail]) :-
    is_pipe_chain([Rest|Tail]).

% Handle traditional two-element pipes [A >> B]
user:b(A, [X, >>, Y], B) :- 
    apply_pipe_step(A, X, C),
    apply_pipe_step(C, Y, B).

% Handle multi-element pipe chains [A >> B >> C >> D >> ...]
user:b(A, Chain, B) :-
    is_list(Chain),
    length(Chain, Len),
    Len >= 5,
    Len mod 2 =:= 1,
    is_pipe_chain(Chain),
    apply_pipe_chain(A, Chain, B).

% Apply a chain of pipe operations
apply_pipe_chain(Input, [SingleOp], Output) :-
    !, 
    apply_pipe_step(Input, SingleOp, Output).

apply_pipe_chain(Input, [FirstOp, >>, SecondOp | Rest], Output) :-
    apply_pipe_step(Input, FirstOp, Intermediate),
    apply_pipe_chain(Intermediate, [SecondOp | Rest], Output).

% Apply a single pipe step (operation)
apply_pipe_step(Input, Operation, Output) :-
    user:builtin(Operation), !,
    user:b(Input, Operation, Output).

apply_pipe_step(Input, Operation, Output) :-
    user:p(Input, Operation, Output).

% Tests
:- begin_tests(pipe_builtin).

test(simple_two_pipe) :-
    % Test [lconcat >> [0]] - concatenate lists then get first element (0-based)
    Input = [["a", "b"], ["c", "d"]],
    b(Input, [lconcat, >>, [0]], Output),
    Output = "a".

test(three_pipe_chain) :-
    % Test [A >> B >> C] 
    Input = [["hello"], ["world"]],
    b(Input, [lconcat, >>, sconcat, >>, [0]], Output),
    Output = 104.  % Character code for 'h'

test(four_pipe_chain) :-
    % Test [A >> B >> C >> D]
    Input = [["hello"], ["world"]],
    b(Input, [lconcat, >>, sconcat, >>, [splitString, "e"], >>, [1]], Output),
    Output = "lloworld".

test(five_pipe_chain) :-
    % Test [A >> B >> C >> D >> E] - even longer chain
    Input = [["abc"], ["def"]],
    b(Input, [lconcat, >>, sconcat, >>, [splitString, "c"], >>, [1], >>, [0]], Output),
    Output = 100.  % Character code for 'd'

:- end_tests(pipe_builtin).
:- set_test_options([silent(false)]).

:- set_prolog_flag(plunit_output, always).

% Run tests and halt
% :- (run_tests -> halt(0) ; halt(1)).