:- module(testlibs, []).

% Test loading HTTP and JSON libraries
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

% If we get here, libraries loaded successfully
:- initialization(test_libs).

test_libs :-
    writeln('SUCCESS: All HTTP/JSON libraries loaded successfully!'),
    writeln('library(http/http_json) - OK'),
    writeln('library(http/json) - OK'),
    halt(0).
