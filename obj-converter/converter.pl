
:- module(converter, [
              convert/0,
              convert/2
          ]).

:- use_module(obj_gen).
:- use_module(obj).

convert :-
    convert("box.obj", "box.sobj"),
    convert("teapot.obj", "teapot.sobj").

convert(Input, Output) :-
    object(Input, Object_Codes),
    string_codes(String, Object_Codes),
    open(Output, write, Stream),
    write(Stream, String),
    close(Stream).

object(Input, Object_Codes) :-
    obj_file(Object, Input),
    generate_object(Object, Object_Codes, []).

