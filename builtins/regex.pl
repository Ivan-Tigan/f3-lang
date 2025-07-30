:- module(regex, []).

:- use_module(library(pcre)).

:- multifile user:p/3.

user:p(Regex, reMatch, String) :-
    re_match(Regex, String).