
:- module(converter, [
              convert/2
          ]).

:- use_module(obj_gen).
:- use_module(obj).

convert(Input, Output) :-
    obj_file(Object, Input),
    object_struct(Object, Codes, []),
    open(Output, write, Stream),
    string_codes(String, Codes),
    write(Stream, String),
    close(Stream).
