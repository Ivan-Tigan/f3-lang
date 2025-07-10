:- module(match, []).

:- multifile user:p/3.
:- multifile p/3.



% Main pattern matching predicate 
user:p(graph(Facts), match, graph(PatternTriples)) :-
    % format(user_error, "Matching facts ~q against pattern ~q~n", [Facts, PatternTriples]),
    match_triples(Facts, PatternTriples),
    % format(user_error, "Match successful~n", []),
    !.

% Base case - empty pattern always matches
match_triples(_, []) :- !.

% Match each triple in pattern
match_triples(Facts, [PatternTriple|Rest]) :-
    match_triple(Facts, PatternTriple),
    match_triples(Facts, Rest).

% Match a single triple pattern using direct unification
match_triple(Facts, p(S, P, O)) :-
    member(p(ActualS, ActualP, ActualO), Facts),
    unify_terms(S, ActualS),
    unify_terms(P, ActualP),
    unify_terms(O, ActualO).

% Handle other goals (e.g. built-ins)
match_triple(_Facts, (Goal)) :-
    call(Goal).

% Direct unification for variables
unify_terms(Var, Value) :-
    var(Var),
    !,
    Var = Value.

% Direct unification for non-graph terms
unify_terms(Pattern, Actual) :-
    \+ Actual = graph(_),
    !,
    Pattern = Actual.

% Unification for graph terms
unify_terms(graph(PatternTriples), graph(ActualFacts)) :-
    !,
    match_triples(ActualFacts, PatternTriples).

% Catch-all for simple unification
unify_terms(Pattern, Pattern).

% Add tests
:- use_module(library(plunit)).

:- begin_tests(match).

test(simple_match) :-
    Facts = [p(a, b, c)],
    Pattern = [p(X, Y, Z)],
    p(graph(Facts), match, graph(Pattern)),
    X = a, Y = b, Z = c.

test(predicate_match) :-
    Facts = [p(a, equals, c)],
    Pattern = [p(_, equals, c)],
    p(graph(Facts), match, graph(Pattern)).

test(nested_match) :-
    Facts = [p(a, has, graph([p(x, y, z)]))],
    Pattern = [p(_, has, graph([p(X, y, z)]))],
    p(graph(Facts), match, graph(Pattern)),
    X = x.

test(deep_variable_binding) :-
    Facts = [
        p(person, data, graph([
            p(name, =, "Alice"),
            p(age, =, 30),
            p(info, =, graph([
                p(city, =, "NYC")
            ]))
        ]))
    ],
    Pattern = [p(person, data, Data)],
    p(graph(Facts), match, graph(Pattern)),
    Data = graph([
        p(name, =, "Alice"),
        p(age, =, 30),
        p(info, =, graph([
            p(city, =, "NYC")
        ]))
    ]).

test(multiple_matches) :-
    Facts = [
        p(a, type, x),
        p(b, type, x),
        p(c, type, y)
    ],
    Pattern = [p(Name, type, x)],
    p(graph(Facts), match, graph(Pattern)),
    memberchk(Name, [a,b]).

:- end_tests(match).

% Run tests
% :- run_tests.
% :- halt.