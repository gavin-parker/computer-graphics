:- module(values, [
              comma_separated_values//1,
              value//1
          ]).

:- use_module(library(dcg/basics)).

comma_separated_values([]) -->
	"",
	!.

comma_separated_values([null|Values]) -->
	!,
	comma_separated_values(Values).

comma_separated_values([Value|Values]) -->
	value(Value),
	!,
	comma_separated_values_tail(Values).


comma_separated_values_tail([]) -->
	"".

comma_separated_values_tail([null|Values]) -->
	!,
	comma_separated_values_tail(Values).

comma_separated_values_tail([Value|Values]) -->
	", ",
	value(Value),
	!,
	comma_separated_values_tail(Values).


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

value(number(N)) -->
	number(N).

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
	"string(\"",
	{
	      value_codes(String, Codes)
	},
	Codes,
	"\")".

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
