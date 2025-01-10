:- use_module(library(chr)).

% Declare CHR constraints
:- chr_constraint node/1, edge/2, color/2, try_color/2.

% Maximum color number (like n=3 in the ASP version)
max_color(3).

% Generate: try to assign a color to each node
try_color(Node, Color) ==> 
    max_color(Max),
    between(1, Max, Color),
    color(Node, Color).

% Test: prevent adjacent nodes from having same color
color(X,C), color(Y,C) ==> edge(X,Y) | fail.

% Helper to setup and run
test :-
    % Nodes
    node(1), node(2), node(3),
    node(4), node(5), node(6),
    
    % Edges (same as ASP version)
    edge(1,2), edge(1,3), edge(1,4),
    edge(2,4), edge(2,5), edge(2,6),
    edge(3,1), edge(3,4), edge(3,5),
    edge(4,1), edge(4,2),
    edge(5,3), edge(5,4), edge(5,6),
    edge(6,2), edge(6,3), edge(6,5),
    
    % Try coloring each node
    try_color(1,_), try_color(2,_), try_color(3,_),
    try_color(4,_), try_color(5,_), try_color(6,_).

% Show colors
show_colors :-
    findall((N,C), color(N,C), Colors),
    sort(Colors, Sorted),
    member((N,C), Sorted),
    format('color(~w,~w).~n', [N,C]),
    fail.
show_colors.