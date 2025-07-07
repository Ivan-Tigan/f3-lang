:- module(lmdb_builtins, []).

:- use_module('../lmdb/src/transactional_triple').
:- use_module(library(plunit)).

:- multifile user:b/3.
:- multifile user:builtin/1.

% Register builtins
user:builtin(lmdbStartR).
user:builtin(lmdbStartRW).
user:builtin(lmdbEnd).
user:builtin(lmdbInsert).
user:builtin(lmdbQuery).

% Transaction management predicates

% Txn lmdbStartR "path" - Start a read-only transaction with custom path
user:b(Txn, lmdbStartR, Path) :-
    lmdbstart(Txn, read, Path).

% Txn lmdbStartRW "path" - Start a read-write transaction with custom path
user:b(Txn, lmdbStartRW, Path) :-
    lmdbstart(Txn, write, Path).

% Txn lmdbEnd [] - End a transaction (commit/abort based on transaction type)
user:b(Txn, lmdbEnd, []) :-
    lmdbend(Txn).

% Triple insertion and querying predicates

% Txn lmdbInsert p(S, P, O) - Insert a triple into the database
% The F3 syntax "Txn lmdbInsert { S P O. }" gets parsed into this
user:b(Txn, lmdbInsert, p(S, P, O)) :-
    lmdbinsert(Txn, S, P, O).

% Txn lmdbQuery p(S, P, O) - Query triples from the database  
% The F3 syntax "Txn lmdbQuery { S P O. }" gets parsed into this
user:b(Txn, lmdbQuery, p(S, P, O)) :-
    lmdbquery(Txn, S, P, O).

% Tests
:- begin_tests(lmdb_builtins).

test(transaction_lifecycle) :-
    b(WriteTxn, lmdbStartRW, "./test_db"),
    b(WriteTxn, lmdbInsert, p(alice, likes, bob)),
    b(WriteTxn, lmdbInsert, p(bob, likes, charlie)),
    b(WriteTxn, lmdbEnd, []),
    
    b(ReadTxn, lmdbStartR, "./test_db"),
    b(ReadTxn, lmdbQuery, p(alice, likes, X)),
    X = bob,
    b(ReadTxn, lmdbQuery, p(Y, likes, charlie)),
    Y = bob,
    b(ReadTxn, lmdbEnd, []).

test(multiple_triple_insert) :-
    b(WriteTxn, lmdbStartRW, "./test_db"),
    b(WriteTxn, lmdbInsert, p(user1, name, "Alice")),
    b(WriteTxn, lmdbInsert, p(user1, age, 30)),
    b(WriteTxn, lmdbInsert, p(user1, city, "NYC")),
    b(WriteTxn, lmdbEnd, []),
    
    b(ReadTxn, lmdbStartR, "./test_db"),
    b(ReadTxn, lmdbQuery, p(user1, name, Name)),
    Name = "Alice",
    b(ReadTxn, lmdbQuery, p(user1, age, Age)),
    Age = 30,
    b(ReadTxn, lmdbEnd, []).

test(variable_pattern_query) :-
    b(WriteTxn, lmdbStartRW, "./test_db"),
    b(WriteTxn, lmdbInsert, p(item1, type, weapon)),
    b(WriteTxn, lmdbInsert, p(item2, type, armor)),
    b(WriteTxn, lmdbInsert, p(item3, type, weapon)),
    b(WriteTxn, lmdbEnd, []),
    
    b(ReadTxn, lmdbStartR, "./test_db"),
    findall(Item, b(ReadTxn, lmdbQuery, p(Item, type, weapon)), Weapons),
    length(Weapons, WeaponCount),
    WeaponCount >= 2,
    b(ReadTxn, lmdbEnd, []).

test(read_only_transaction) :-
    % Insert some test data first
    b(WriteTxn, lmdbStartRW, "./test_db"),
    b(WriteTxn, lmdbInsert, p(test, readonly, value)),
    b(WriteTxn, lmdbEnd, []),
    
    % Test read-only access
    b(ReadTxn, lmdbStartR, "./test_db"),
    b(ReadTxn, lmdbQuery, p(test, readonly, Result)),
    Result = value,
    b(ReadTxn, lmdbEnd, []).

test(complex_graph_traversal) :-
    % Set up a small graph
    b(WriteTxn, lmdbStartRW, "./test_db"),
    b(WriteTxn, lmdbInsert, p(a, follows, b)),
    b(WriteTxn, lmdbInsert, p(b, follows, c)),
    b(WriteTxn, lmdbInsert, p(c, popularity, 100)),
    b(WriteTxn, lmdbEnd, []),
    
    % Query the graph
    b(ReadTxn, lmdbStartR, "./test_db"),
    
    % Find who 'a' follows
    b(ReadTxn, lmdbQuery, p(a, follows, FirstTarget)),
    FirstTarget = b,
    
    % Find who that person follows
    b(ReadTxn, lmdbQuery, p(FirstTarget, follows, SecondTarget)),
    SecondTarget = c,
    
    % Find their popularity
    b(ReadTxn, lmdbQuery, p(SecondTarget, popularity, Pop)),
    Pop = 100,
    
    b(ReadTxn, lmdbEnd, []).

test(f3_style_rule_with_findall) :-
    % Define a rule similar to what F3 generates
    assertz((result_inserted(ok) :-
        b(Txn, lmdbStartRW, "./test_db"),
        b(Txn, lmdbInsert, p(alice, likes, programming)),
        b(Txn, lmdbEnd, [])
    )),
    
    % Use findall like F3's system query does
    findall(X, result_inserted(X), Results),
    member(ok, Results),
    
    % Clean up
    retract((result_inserted(_) :- _)).

test(f3_style_query_rule_with_findall) :-
    % First insert some data
    b(WriteTxn, lmdbStartRW, "./test_db"),
    b(WriteTxn, lmdbInsert, p(alice, likes, programming)),
    b(WriteTxn, lmdbEnd, []),
    
    % Define a rule that queries (like F3 generates)
    assertz((result_found(X) :-
        b(ReadTxn, lmdbStartR, "./test_db"),
        b(ReadTxn, lmdbQuery, p(alice, likes, X)),
        b(ReadTxn, lmdbEnd, [])
    )),
    
    % Use findall to query it
    findall(X, result_found(X), Results),
    member(programming, Results),
    
    % Clean up
    retract((result_found(_) :- _)).

:- end_tests(lmdb_builtins).

% Run tests
:- set_test_options([silent(false)]).