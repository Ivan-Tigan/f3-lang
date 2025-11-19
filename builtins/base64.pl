:- module(base64_builtin, []).

:- use_module(library(base64)).
:- use_module(library(plunit)).
:- use_module(library(pcre)).

:- multifile user:p/3.

% Set test options to show output
:- set_test_options([silent(false)]).
% Bidirectional standard base64 encoding/decoding
user:p(Plain, base64, Encoded) :-
    var(Plain), nonvar(Encoded), !,
    % Decoding: convert string to atom if needed
    (string(Encoded) -> atom_string(EncodedAtom, Encoded) ; EncodedAtom = Encoded),
    base64(PlainAtom, EncodedAtom),
    (string(Encoded) -> atom_string(PlainAtom, Plain) ; Plain = PlainAtom).

user:p(Plain, base64, Encoded) :-
    nonvar(Plain), var(Encoded), !,
    % Encoding: convert string to atom if needed
    (string(Plain) -> atom_string(PlainAtom, Plain) ; PlainAtom = Plain),
    base64(PlainAtom, EncodedAtom),
    (string(Plain) -> atom_string(EncodedAtom, Encoded) ; Encoded = EncodedAtom).

% Bidirectional base64URL encoding/decoding (used by JWT)
user:p(Plain, base64Url, Encoded) :-
    var(Plain), nonvar(Encoded), !,
    % Decoding: convert base64url to base64, then decode
    (string(Encoded) -> atom_string(EncodedAtom, Encoded) ; EncodedAtom = Encoded),
    atom_string(EncodedAtom, EncodedStr),
    % Replace URL-safe chars: - with +, _ with /
    re_replace("-", "+", EncodedStr, WithPlus, [global]),
    re_replace("_", "/", WithPlus, WithSlash, [global]),
    % Add padding if needed
    atom_length(WithSlash, Len),
    PadNeeded is (4 - (Len mod 4)) mod 4,
    (PadNeeded =:= 0 ->
        Base64 = WithSlash
    ;   format(atom(Padding), '~*c', [PadNeeded, 0'=]),
        atom_concat(WithSlash, Padding, Base64)
    ),
    base64(PlainAtom, Base64),
    (string(Encoded) -> atom_string(PlainAtom, Plain) ; Plain = PlainAtom).

user:p(Plain, base64Url, Encoded) :-
    nonvar(Plain), var(Encoded), !,
    % Encoding: encode to base64, then convert to base64url
    (string(Plain) -> atom_string(PlainAtom, Plain) ; PlainAtom = Plain),
    base64(PlainAtom, Base64),
    % Remove padding and replace chars: + with -, / with _
    re_replace("=+$", "", Base64, NoPadding, []),
    re_replace("\\+", "-", NoPadding, WithMinus, [global]),
    re_replace("/", "_", WithMinus, EncodedAtom, [global]),
    (string(Plain) -> atom_string(EncodedAtom, Encoded) ; Encoded = EncodedAtom).

% Tests
:- begin_tests(base64_builtin).

test(base64_encode) :-
    p("Hello World", base64, Encoded),
    Encoded = "SGVsbG8gV29ybGQ=".

test(base64_decode) :-
    p(Plain, base64, "SGVsbG8gV29ybGQ="),
    Plain = "Hello World".

test(base64_bidirectional) :-
    TestString = "Test string for encoding",
    p(TestString, base64, Encoded),
    p(Decoded, base64, Encoded),
    TestString = Decoded.

test(base64url_encode) :-
    p("Hello World", base64Url, Encoded),
    % base64url should use URL-safe chars (no + or /)
    \+ sub_string(Encoded, _, _, _, "+"),
    \+ sub_string(Encoded, _, _, _, "/").

test(base64url_decode) :-
    % Test with a known base64url string
    p("Hello World", base64Url, Encoded),
    p(Decoded, base64Url, Encoded),
    Decoded = "Hello World".

test(base64url_bidirectional) :-
    TestString = "JWT payload test string",
    p(TestString, base64Url, Encoded),
    p(Decoded, base64Url, Encoded),
    TestString = Decoded.

test(jwt_payload_decode) :-
    % Real JWT payload from Logto
    JwtPayload = "eyJzdWIiOiJzdTEwNnk3bTZoeWwiLCJuYW1lIjoiSXZhbiBUc29uaW5za2kiLCJwaWN0dXJlIjoiaHR0cHM6Ly9saDMuZ29vZ2xldXNlcmNvbnRlbnQuY29tL2EvQUNnOG9jSVhQcHF2RkhpZklXczJSS0pEOXR6UDg0QVNCRDByZUZNTkx3b1hiSHNHVDNaallRPXM5Ni1jIiwidXBkYXRlZF9hdCI6MTc1MTUzNzg2MDc5MSwidXNlcm5hbWUiOiJJdmFuIiwiY3JlYXRlZF9hdCI6MTc0MDAwNTY1ODY4MiwiZW1haWwiOiJpdmFudHNvbmluc2tpQGdtYWlsLmNvbSIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJhdXRoX3RpbWUiOjE3NTE1Mzc4NjAsImF0X2hhc2giOiJYLWZITVU3MTAtWDJ3Q3dPRno0QXh5YW5jb0luSm1OMiIsImF1ZCI6IjB4ZHJqZ25mdmpkcG43d2ZnczF6MiIsImV4cCI6MTc1MTU0MTQ2MSwiaWF0IjoxNzUxNTM3ODYxLCJpc3MiOiJodHRwczovL2xvZ3RvLmYzd2ViLmFwcC9vaWRjIn0",
    p(Decoded, base64Url, JwtPayload),
    % Should decode to JSON string containing user info
    sub_string(Decoded, _, _, _, "Ivan Tsoninski"),
    sub_string(Decoded, _, _, _, "ivantsoninski@gmail.com").

:- end_tests(base64_builtin).
:- set_test_options([silent(false)]).

:- set_prolog_flag(plunit_output, always).

% Run tests and halt
% :- (run_tests -> halt(0) ; halt(1)).