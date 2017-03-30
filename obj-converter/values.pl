:- module(values, [
	      space_separated_values//1,
              comma_separated_values//1
          ]).

:- use_module(library(dcg/basics)).

space_separated_values(Values) -->
	separated_values(" ", Values).


comma_separated_values(Values) -->
	separated_values(", ", Values).


separated_values(_Separator, []) -->
	"",
	!.

separated_values(Separator, [eol|Values]) -->
	value(eol),
	!,
	separated_values(Separator, Values).

separated_values(Separator, [Value|Values]) -->
	value(Value),
	!,
	separated_values_tail(Separator, Values).


separated_values_tail(_Separator, []) -->
	"",
	!.

separated_values_tail(Separator, [eol|Values]) -->
	value(eol),
	!,
	separated_values_tail(Separator, Values).

separated_values_tail(Separator, [Value|Values]) -->
	Separator,
	value(Value),
	!,
	separated_values_tail(Separator, Values).



vec2(X, Y) -->
	"vec2(",
	comma_separated_values([number(X), number(Y)]),
	")".


vec3(X, Y, Z) -->
	"vec3(",
	comma_separated_values([number(X), number(Y), number(Z)]),
	")".


vec4(X, Y, Z, W) -->
	"vec4(",
	comma_separated_values([number(X), number(Y), number(Z), number(W)]),
	")".

value(null) -->
	"null".

value(eol) -->
	"\n".

value(number(N)) -->
	number(N).

value(list_count(List), Codes, Rest) :-
	length(List, Length),
	value(number(Length), Codes, Rest).

value(vt(U, V)) -->
	vec2(U, V).

value(vec4(X, Y, Z, W)) -->
	vec4(X, Y, Z, W).

value(v(X, Y, Z)) -->
	vec3(X, Y, Z).

value(v(X, Y, Z, W)) -->
	vec4(X, Y, Z, W).

value(vt(X, Y)) -->
	vec2(X, Y).

value(vn(X, Y, Z)) -->
	vec3(X, Y, Z).

value(vn(X, Y, Z, W)) -->
	vec4(X, Y, Z, W).

value(rgb(R, G, B)) -->
	vec3(R, G, B).

value(file(File)) -->
	value(string(File)).

value(string(String)) -->
	"\"",
	{
	      value_codes(String, Codes)
	},
	Codes,
	"\"".

value(bool(true)) -->
	"true".

value(bool(false)) -->
	"false".

value_codes(Value, Codes) :-
	atom(Value),
	atom_codes(Value, Codes).

value_codes(Value, Codes) :-
	string(Value),
	string_codes(Value, Codes).
