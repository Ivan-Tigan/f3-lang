:- include('p.pl').
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).

% HTTP handler setup - note the [prefix] option
:- http_handler('/', handle_request, [prefix]).

% Start the server
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Handle incoming requests 
handle_request(Request) :-
    % Get the path from request
    memberchk(path(Path), Request),
    % Remove leading slash
    atom_concat('/', RelPath, Path),
    
    % Debug output to see what we're looking for
    format(user_error, "Looking for path: ~w~n", [RelPath]),
    
    % Find matching content
    (   p(res, is, l(RelPath, Content))
    ->  format('Content-type: text/html~n~n'),
        write(Content)
    ;   throw(http_reply(not_found(Path)))
    ).

% Add error handling
:- multifile http:status_reply/3.