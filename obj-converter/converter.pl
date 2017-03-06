
:- module(converter, [
              convert/2,
              convert/3
          ]).

:- use_module(obj_gen).
:- use_module(obj).

convert(Objects, Output) :-
    open(Output, write, Stream),
    objects_file(Objects, Codes, []),
    string_codes(String, Codes),
    write(Stream, String),
    close(Stream).

convert(Name, Input, Output) :-
    convert([Name-Input], Output).

objects_file(Objects) -->
    "#include \"objects.h\";\n\n",
    objects(Objects).

objects([]) -->
    "".

objects([Name-Input|Objects]) -->
    object(Name, Input),
    !,
    objects(Objects).

object(Name, Input, Codes, Rest) :-
    obj_file(Object, Input),
    generate_object(Name, Object, Codes, Rest).
