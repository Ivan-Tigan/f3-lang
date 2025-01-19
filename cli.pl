:- use_module(library(process)).

run_command(Command, Output) :-
    process_create(path(sh), ['-c', Command],
        [stdout(pipe(Out))]),
    read_string(Out, _, Output),
    close(Out).