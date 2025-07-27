% banana.pl - Simple PNG byte reader

read_banana :-
    open('Banana.png', read, Stream, [type(binary)]),
    read_all_bytes(Stream, Bytes),
    close(Stream),
    write(Bytes).

read_all_bytes(Stream, Bytes) :-
    read_all_bytes(Stream, [], Bytes).

read_all_bytes(Stream, Acc, Bytes) :-
    get_byte(Stream, Byte),
    (   Byte == -1
    ->  reverse(Acc, Bytes)
    ;   read_all_bytes(Stream, [Byte|Acc], Bytes)
    ).