:- module(util, [
	      white_integer//1,
	      white_number//1,
	      white_maybe_number//2,
	      comment//0,
	      white_eol//0,
	      eol//0
	  ]).

:- use_module(library(dcg/basics)).

white_integer(N) -->
    white,
    whites,
    integer(N).


white_number(N) -->
    white,
    whites,
    number(N).


white_maybe_number(N, _Default) -->
    white,
    whites,
    number(N).

white_maybe_number(Default, Default) -->
    "".

comment -->
    "#",
    string_without("\r\n", _Line),
    eol.

white_eol -->
    whites,
    eol.


eol -->
    "\r\n".

eol -->
    "\r".

eol -->
    "\n".
