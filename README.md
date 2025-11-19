# F3 Programming Language

![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
![Version](https://img.shields.io/badge/version-1.0.0-blue)
![Platform](https://img.shields.io/badge/platform-linux-lightgrey)
![Status](https://img.shields.io/badge/status-experimental-orange)

A simple, powerful, extensible logic programming language based on triples.



## Install

Requires `docker`

``` 
wget -qO- https://raw.githubusercontent.com/Ivan-Tigan/f3-lang/main/install.sh | sudo bash
```
## Get Example

this will download the ./example folder from the repository to your machine

```
wget -qO- https://api.github.com/repos/Ivan-Tigan/f3-lang/tarball | tar xz --wildcards --strip=1 "*/example"
```
```
cd example
```

## Try It

```
f3 -p 3000:3000 run todo.f3
```

## Language Overview

F3 combines logic programming with modern features: persistent triple store (LMDB), constraint solving (CLPFD/CLPQR), HTTP/WebSocket servers, and a declarative syntax. Programs are written as triples with backward-chaining rules for inference.

## Language Examples

### Basic Triple Syntax

```f3
// Simple facts
bob name "Robert".
bob age 30.
alice likes bob, charlie.  // Multiple objects: bob and charlie

// Rules: (condition) => (conclusion)
(?X likes ?Y. ?Y likes ?Z.) => (?X friendOf ?Z.).
```

### Datatypes

```f3
// Atoms (lowercase) and strings
user1 id myuser.
user1 name "John Doe".

// Numbers
price value 29.99.
quantity value 5.

// Lists
cart items [apple banana orange].
coords point [10 20 30].

// Chains (colon-separated values)
query is sql:["select * from " table " where size > " 30].
distance is 10:meters.
time is 05:03:2025:UTC.
[1 2 3] mod1:sum3 ?result.  // Namespaced predicates
```

### Graphs vs Nested Triples

```f3
// Nested triple (5-tuple) - uses {}
db hasGraph {
    user1 name "Alice".
    user1 role "admin".
}.

// Graph in rules - uses ()
(?Txn lmdbQuery { ?UserId role "admin". }.) => (?UserId hasAdminAccess true.).

// Graph as parameter
system prove (?X likes ?Y. ?Y age ?Age.).
```

### Graph Database (LMDB)

```f3
// Insert data
?Txn lmdbStartRW "./db"
; lmdbInsert {
    user123 email "user@example.com".
    user123 role "admin".
}
; lmdbEnd [].

// Query with pattern matching
?Txn lmdbStartR "./db"
; lmdbQuery {
    ?BookingId a Booking
    ; email ?UserEmail
    ; event ?EventId.
}
; lmdbEnd [].
```

### Constraint Solving (CLPFD)

```f3
// Find solutions where X + Y = 10
system query [[result X ?X Y ?Y] (
    clpfd constraint [
        ?X + ?Y = 10
        & ?X in 1 5
        & ?Y in 3 8
    ];
    label [?X ?Y].
)].

// Scheduling with time constraints
?EventStartT [+ ?Iteration * ?SecondsPerWeek] ?EventIterationStartT.
clpfd constraint [
    ?T1 + 2 =< ?T2  // Task 1 takes 2 time units
    & ?T2 + 1 =< ?T3
    & ?T3 =< 4
].
```

See `/example` for examples.

