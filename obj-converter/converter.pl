
:- module(converter, [
              convert/2
          ]).

:- use_module(cpp_gen).
:- use_module(obj).

convert(File, String) :-
    obj_file(Object, File),
    object_struct(Object, Codes, []),
    string_codes(String, Codes).
