:- module(lmdb_builtins, []).

:- use_module('../lmdb/src/transactional_triple').
:- use_module(library(plunit)).

:- multifile user:p/3.

% Register builtins







% Transaction management predicates

% Txn lmdbStartR "path" - Start a read-only transaction with custom path
user:p(Txn, lmdbStartR, Path) :-
    lmdbstart(Txn, read, Path).

% Txn lmdbStartRW "path" - Start a read-write transaction with custom path
user:p(Txn, lmdbStartRW, Path) :-
    lmdbstart(Txn, write, Path).

% Txn lmdbEnd [] - End a transaction (commit/abort based on transaction type)
user:p(Txn, lmdbEnd, []) :-
    lmdbend(Txn).

% Triple insertion and querying predicates

% Txn lmdbInsert p(S, P, O) - Insert a triple into the database
% The F3 syntax "Txn lmdbInsert { S P O. }" gets parsed into this
user:p(Txn, lmdbInsert, p(S, P, O)) :-
    lmdbinsert(Txn, S, P, O).

% Txn lmdbQuery p(S, P, O) - Query triples from the database  
% The F3 syntax "Txn lmdbQuery { S P O. }" gets parsed into this
user:p(Txn, lmdbQuery, p(S, P, O)) :-
    lmdbquery(Txn, S, P, O).

% Txn lmdbDelete p(S, P, O) - Delete a triple from the database
% The F3 syntax "Txn lmdbDelete { S P O. }" gets parsed into this
user:p(Txn, lmdbDelete, p(S, P, O)) :-
    lmdbdelete(Txn, S, P, O).

% Tests
:- begin_tests(lmdb_builtins).

test(transaction_lifecycle) :-
    p(WriteTxn, lmdbStartRW, "./test_db"),
    p(WriteTxn, lmdbInsert, p(alice, likes, bob)),
    p(WriteTxn, lmdbInsert, p(bob, likes, charlie)),
    p(WriteTxn, lmdbEnd, []),
    
    p(ReadTxn, lmdbStartR, "./test_db"),
    p(ReadTxn, lmdbQuery, p(alice, likes, X)),
    X = bob,
    p(ReadTxn, lmdbQuery, p(Y, likes, charlie)),
    Y = bob,
    p(ReadTxn, lmdbEnd, []).

test(multiple_triple_insert) :-
    p(WriteTxn, lmdbStartRW, "./test_db"),
    p(WriteTxn, lmdbInsert, p(user1, name, "Alice")),
    p(WriteTxn, lmdbInsert, p(user1, age, 30)),
    p(WriteTxn, lmdbInsert, p(user1, city, "NYC")),
    p(WriteTxn, lmdbEnd, []),
    
    p(ReadTxn, lmdbStartR, "./test_db"),
    p(ReadTxn, lmdbQuery, p(user1, name, Name)),
    Name = "Alice",
    p(ReadTxn, lmdbQuery, p(user1, age, Age)),
    Age = 30,
    p(ReadTxn, lmdbEnd, []).

test(variable_pattern_query) :-
    p(WriteTxn, lmdbStartRW, "./test_db"),
    p(WriteTxn, lmdbInsert, p(item1, type, weapon)),
    p(WriteTxn, lmdbInsert, p(item2, type, armor)),
    p(WriteTxn, lmdbInsert, p(item3, type, weapon)),
    p(WriteTxn, lmdbEnd, []),
    
    p(ReadTxn, lmdbStartR, "./test_db"),
    findall(Item, p(ReadTxn, lmdbQuery, p(Item, type, weapon)), Weapons),
    length(Weapons, WeaponCount),
    WeaponCount >= 2,
    p(ReadTxn, lmdbEnd, []).

test(read_only_transaction) :-
    % Insert some test data first
    p(WriteTxn, lmdbStartRW, "./test_db"),
    p(WriteTxn, lmdbInsert, p(test, readonly, value)),
    p(WriteTxn, lmdbEnd, []),
    
    % Test read-only access
    p(ReadTxn, lmdbStartR, "./test_db"),
    p(ReadTxn, lmdbQuery, p(test, readonly, Result)),
    Result = value,
    p(ReadTxn, lmdbEnd, []).

test(complex_graph_traversal) :-
    % Set up a small graph
    p(WriteTxn, lmdbStartRW, "./test_db"),
    p(WriteTxn, lmdbInsert, p(a, follows, b)),
    p(WriteTxn, lmdbInsert, p(b, follows, c)),
    p(WriteTxn, lmdbInsert, p(c, popularity, 100)),
    p(WriteTxn, lmdbEnd, []),
    
    % Query the graph
    p(ReadTxn, lmdbStartR, "./test_db"),
    
    % Find who 'a' follows
    p(ReadTxn, lmdbQuery, p(a, follows, FirstTarget)),
    FirstTarget = b,
    
    % Find who that person follows
    p(ReadTxn, lmdbQuery, p(FirstTarget, follows, SecondTarget)),
    SecondTarget = c,
    
    % Find their popularity
    p(ReadTxn, lmdbQuery, p(SecondTarget, popularity, Pop)),
    Pop = 100,
    
    p(ReadTxn, lmdbEnd, []).

test(f3_style_rule_with_findall) :-
    % Define a rule similar to what F3 generates
    assertz((result_inserted(ok) :-
        p(Txn, lmdbStartRW, "./test_db"),
        p(Txn, lmdbInsert, p(alice, likes, programming)),
        p(Txn, lmdbEnd, [])
    )),
    
    % Use findall like F3's system query does
    findall(X, result_inserted(X), Results),
    member(ok, Results),
    
    % Clean up
    retract((result_inserted(_) :- _)).

test(f3_style_query_rule_with_findall) :-
    % First insert some data
    p(WriteTxn, lmdbStartRW, "./test_db"),
    p(WriteTxn, lmdbInsert, p(alice, likes, programming)),
    p(WriteTxn, lmdbEnd, []),
    
    % Define a rule that queries (like F3 generates)
    assertz((result_found(X) :-
        p(ReadTxn, lmdbStartR, "./test_db"),
        p(ReadTxn, lmdbQuery, p(alice, likes, X)),
        p(ReadTxn, lmdbEnd, [])
    )),
    
    % Use findall to query it
    findall(X, result_found(X), Results),
    member(programming, Results),
    
    % Clean up
    retract((result_found(_) :- _)).

:- end_tests(lmdb_builtins).

% Run tests
:- set_test_options([silent(false)]).