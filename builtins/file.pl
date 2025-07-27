:- module(file_builtin, []).

:- use_module(library(plunit)).

% Set test options to show output
:- set_test_options([silent(false)]).

:- multifile user:p/3.

% Read file as text string
user:p(RelativeFilePath, readFileAsText, FileContentsString) :-
    atom_string(RelativeFilePath, FilePathString),
    read_file_to_string(FilePathString, FileContentsString, []).

% Read file as byte array
user:p(RelativeFilePath, readFileAsBytes, FileContentsByteArray) :-
    atom_string(RelativeFilePath, FilePathString),
    open(FilePathString, read, Stream, [type(binary)]),
    read_all_bytes(Stream, FileContentsByteArray),
    close(Stream).

% Write text to file
user:p(RelativeFilePath, writeTextToFile, TextContents) :-
    atom_string(RelativeFilePath, FilePathString),
    % Ensure directory exists
    file_directory_name(FilePathString, Directory),
    (exists_directory(Directory) -> 
        true 
    ;   make_directory_path(Directory)
    ),
    open(FilePathString, write, Stream),
    write(Stream, TextContents),
    close(Stream).

% Write bytes to file
user:p(RelativeFilePath, writeBytesToFile, ByteArray) :-
    atom_string(RelativeFilePath, FilePathString),
    % Ensure directory exists
    file_directory_name(FilePathString, Directory),
    (exists_directory(Directory) -> 
        true 
    ;   make_directory_path(Directory)
    ),
    open(FilePathString, write, Stream, [type(binary)]),
    write_all_bytes(Stream, ByteArray),
    close(Stream).

% Helper predicate to write all bytes to a stream
write_all_bytes(_, []).
write_all_bytes(Stream, [Byte|Rest]) :-
    put_byte(Stream, Byte),
    write_all_bytes(Stream, Rest).

% Helper predicate to read all bytes from a stream
read_all_bytes(Stream, Bytes) :-
    read_all_bytes(Stream, [], Bytes).

read_all_bytes(Stream, Acc, Bytes) :-
    get_byte(Stream, Byte),
    (   Byte == -1
    ->  reverse(Acc, Bytes)
    ;   read_all_bytes(Stream, [Byte|Acc], Bytes)
    ).

% Tests
:- begin_tests(file_builtin).

test(read_text_file) :-
    % Create a test file first
    TestFile = 'test_file.txt',
    open(TestFile, write, Stream),
    write(Stream, 'Hello, World!\nThis is a test file.'),
    close(Stream),
    
    % Test reading it
    p(TestFile, readFileAsText, Contents),
    Contents = 'Hello, World!\nThis is a test file.',
    
    % Clean up
    delete_file(TestFile).

test(read_bytes_file) :-
    % Create a test file with known bytes
    TestFile = 'test_bytes.bin',
    open(TestFile, write, Stream, [type(binary)]),
    put_byte(Stream, 72),  % 'H'
    put_byte(Stream, 101), % 'e'
    put_byte(Stream, 108), % 'l'
    put_byte(Stream, 108), % 'l'
    put_byte(Stream, 111), % 'o'
    close(Stream),
    
    % Test reading it
    p(TestFile, readFileAsBytes, Bytes),
    Bytes = [72, 101, 108, 108, 111],
    
    % Clean up
    delete_file(TestFile).

test(read_nonexistent_file) :-
    % Test error handling for nonexistent file
    catch(
        p('nonexistent_file.txt', readFileAsText, _),
        Error,
        Error = error(existence_error(source_sink, 'nonexistent_file.txt'), _)
    ).

test(write_text_file) :-
    % Test writing text to file
    TestFile = 'test_write.txt',
    TestContent = 'Hello, World!\nThis is written text.',
    p(TestFile, writeTextToFile, TestContent),
    
    % Verify it was written correctly
    p(TestFile, readFileAsText, ReadContent),
    TestContent = ReadContent,
    
    % Clean up
    delete_file(TestFile).

test(write_bytes_file) :-
    % Test writing bytes to file
    TestFile = 'test_write_bytes.bin',
    TestBytes = [72, 101, 108, 108, 111],  % 'Hello'
    p(TestFile, writeBytesToFile, TestBytes),
    
    % Verify it was written correctly
    p(TestFile, readFileAsBytes, ReadBytes),
    TestBytes = ReadBytes,
    
    % Clean up
    delete_file(TestFile).

test(write_file_with_directory) :-
    % Test writing to a file in a subdirectory
    TestFile = 'test_dir/subdir/test.txt',
    TestContent = 'File in subdirectory',
    p(TestFile, writeTextToFile, TestContent),
    
    % Verify it was written correctly
    p(TestFile, readFileAsText, ReadContent),
    TestContent = ReadContent,
    
    % Clean up
    delete_file(TestFile),
    delete_directory('test_dir/subdir'),
    delete_directory('test_dir').

:- end_tests(file_builtin).
:- set_test_options([silent(false)]).

:- set_prolog_flag(plunit_output, always).

% Run tests and halt
% :- (run_tests -> halt(0) ; halt(1)).