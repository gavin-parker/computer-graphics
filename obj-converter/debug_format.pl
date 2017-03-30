
:- module(debug_format, [
              debug_format/2
          ]).

debug_format(Format, Arguments) :-
    (   tracing
    ->  format(Format, Arguments)
    ;   true).
