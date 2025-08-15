:- module(ziplib, []).

:- use_module(library(zlib)).

:- multifile user:p/3.

% Main predicate for gunzip decompression
user:p(Input, gunzip, Output) :-
    % Handle different input formats
    (   is_list(Input) ->
        % Input is a list of byte codes
        InputCodes = Input
    ;   atom(Input) ->
        % Input is an atom, convert to codes
        atom_codes(Input, InputCodes)
    ;   string(Input) ->
        % Input is a string, convert to codes
        string_codes(Input, InputCodes)
    ;   % Unknown input format
        throw(error(type_error(gzip_input, Input), context(gunzip/2, 'Input must be atom, string, or list of codes')))
    ),
    
    % Decompress using zlib - create a stream from codes first
    setup_call_cleanup(
        open_codes_stream(InputCodes, InputStream),
        (   setup_call_cleanup(
                zopen(InputStream, ZStream, [format(gzip), close_parent(false)]),
                read_stream_to_codes(ZStream, OutputCodes),
                close(ZStream)
            )
        ),
        close(InputStream)
    ),
    
    % Convert output codes to atom
    string_codes(Output, OutputCodes).

% Alternative predicate that preserves output as codes
user:p(Input, gunzip_codes, Output) :-
    % Handle different input formats
    (   is_list(Input) ->
        % Input is a list of byte codes
        InputCodes = Input
    ;   atom(Input) ->
        % Input is an atom, convert to codes
        atom_codes(Input, InputCodes)
    ;   string(Input) ->
        % Input is a string, convert to codes
        string_codes(Input, InputCodes)
    ;   % Unknown input format
        throw(error(type_error(gzip_input, Input), context(gunzip_codes/2, 'Input must be atom, string, or list of codes')))
    ),
    
    % Decompress using zlib - create a stream from codes first
    setup_call_cleanup(
        open_codes_stream(InputCodes, InputStream),
        (   setup_call_cleanup(
                zopen(InputStream, ZStream, [format(gzip), close_parent(false)]),
                read_stream_to_codes(ZStream, Output),
                close(ZStream)
            )
        ),
        close(InputStream)
    ).