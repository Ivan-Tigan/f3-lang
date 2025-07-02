:- module(lmdb_builtins, []).

:- use_module('../lmdb/src/transactional_triple').
:- use_module(library(plunit)).

:- multifile user:b/3.
:- multifile user:builtin/1.

% Register builtins
user:builtin(readStart).
user:builtin(readWriteStart).
user:builtin(end).
user:builtin(lmdbInsert).
user:builtin(lmdbQuery).

% Transaction management predicates

% lmdb readStart Txn - Start a read-only transaction
user:b(lmdb, readStart, Txn) :-
    init_store,
    lmdbstart(Txn, read).

% lmdb readWriteStart Txn - Start a read-write transaction
user:b(lmdb, readWriteStart, Txn) :-
    init_store,
    lmdbstart(Txn, write).

% lmdb end Txn - End a transaction (commit/abort based on transaction type)
user:b(lmdb, end, Txn) :-
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
    b(lmdb, readWriteStart, WriteTxn),
    b(WriteTxn, lmdbInsert, p(alice, likes, bob)),
    b(WriteTxn, lmdbInsert, p(bob, likes, charlie)),
    b(lmdb, end, WriteTxn),
    
    b(lmdb, readStart, ReadTxn),
    b(ReadTxn, lmdbQuery, p(alice, likes, X)),
    X = bob,
    b(ReadTxn, lmdbQuery, p(Y, likes, charlie)),
    Y = bob,
    b(lmdb, end, ReadTxn).

test(multiple_triple_insert) :-
    b(lmdb, readWriteStart, WriteTxn),
    b(WriteTxn, lmdbInsert, p(user1, name, "Alice")),
    b(WriteTxn, lmdbInsert, p(user1, age, 30)),
    b(WriteTxn, lmdbInsert, p(user1, city, "NYC")),
    b(lmdb, end, WriteTxn),
    
    b(lmdb, readStart, ReadTxn),
    b(ReadTxn, lmdbQuery, p(user1, name, Name)),
    Name = "Alice",
    b(ReadTxn, lmdbQuery, p(user1, age, Age)),
    Age = 30,
    b(lmdb, end, ReadTxn).

test(variable_pattern_query) :-
    b(lmdb, readWriteStart, WriteTxn),
    b(WriteTxn, lmdbInsert, p(item1, type, weapon)),
    b(WriteTxn, lmdbInsert, p(item2, type, armor)),
    b(WriteTxn, lmdbInsert, p(item3, type, weapon)),
    b(lmdb, end, WriteTxn),
    
    b(lmdb, readStart, ReadTxn),
    findall(Item, b(ReadTxn, lmdbQuery, p(Item, type, weapon)), Weapons),
    length(Weapons, WeaponCount),
    WeaponCount >= 2,
    b(lmdb, end, ReadTxn).

test(read_only_transaction) :-
    % Insert some test data first
    b(lmdb, readWriteStart, WriteTxn),
    b(WriteTxn, lmdbInsert, p(test, readonly, value)),
    b(lmdb, end, WriteTxn),
    
    % Test read-only access
    b(lmdb, readStart, ReadTxn),
    b(ReadTxn, lmdbQuery, p(test, readonly, Result)),
    Result = value,
    b(lmdb, end, ReadTxn).

test(complex_graph_traversal) :-
    % Set up a small graph
    b(lmdb, readWriteStart, WriteTxn),
    b(WriteTxn, lmdbInsert, p(a, follows, b)),
    b(WriteTxn, lmdbInsert, p(b, follows, c)),
    b(WriteTxn, lmdbInsert, p(c, popularity, 100)),
    b(lmdb, end, WriteTxn),
    
    % Query the graph
    b(lmdb, readStart, ReadTxn),
    
    % Find who 'a' follows
    b(ReadTxn, lmdbQuery, p(a, follows, FirstTarget)),
    FirstTarget = b,
    
    % Find who that person follows
    b(ReadTxn, lmdbQuery, p(FirstTarget, follows, SecondTarget)),
    SecondTarget = c,
    
    % Find their popularity
    b(ReadTxn, lmdbQuery, p(SecondTarget, popularity, Pop)),
    Pop = 100,
    
    b(lmdb, end, ReadTxn).

test(f3_style_rule_with_findall) :-
    % Define a rule similar to what F3 generates
    assertz((result_inserted(ok) :-
        b(lmdb, readWriteStart, Txn),
        b(Txn, lmdbInsert, p(alice, likes, programming)),
        b(lmdb, end, Txn)
    )),
    
    % Use findall like F3's system query does
    findall(X, result_inserted(X), Results),
    member(ok, Results),
    
    % Clean up
    retract((result_inserted(_) :- _)).

test(f3_style_query_rule_with_findall) :-
    % First insert some data
    b(lmdb, readWriteStart, WriteTxn),
    b(WriteTxn, lmdbInsert, p(alice, likes, programming)),
    b(lmdb, end, WriteTxn),
    
    % Define a rule that queries (like F3 generates)
    assertz((result_found(X) :-
        b(lmdb, readStart, ReadTxn),
        b(ReadTxn, lmdbQuery, p(alice, likes, X)),
        b(lmdb, end, ReadTxn)
    )),
    
    % Use findall to query it
    findall(X, result_found(X), Results),
    member(programming, Results),
    
    % Clean up
    retract((result_found(_) :- _)).

:- end_tests(lmdb_builtins).

% Run tests
:- set_test_options([silent(false)]).