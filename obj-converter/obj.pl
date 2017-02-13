
:- module(obj, [
              obj_file/2,
              obj//1
          ]).

:- use_module(library(dcg/basics)).
:- use_module(library(pio)).


obj_file(Object, File) :-
    phrase_from_file(obj(Object), File).

obj(Object, Codes, End) :-
    initial_state(Initial_State),
    read_lines(Initial_State, Final_State, Codes, End),
    final_state(Final_State, Object).


initial_state(o(V-V, T-T, N-N, F-F)).


final_state(o(V-[], T-[], N-[], F-[]), o(V, T, N, F)).


read_lines(State, State) -->
    eos,
    !.

read_lines(Initial_State, Final_State) -->
    read_line(Initial_State, Intermediate_State),
    !,
    read_lines(Intermediate_State, Final_State).


read_line(o(Vs0, Ts, Ns, Fs), o(Vs1, Ts, Ns, Fs)) -->
    vertex(V),
    {
        diff_list_append(Vs0, V, Vs1)
    }.

read_line(o(Vs, Ts0, Ns, Fs), o(Vs, Ts1, Ns, Fs)) -->
    texture_coordinate(T),
    {
        diff_list_append(Ts0, T, Ts1)
    }.

read_line(o(Vs, Ts, Ns0, Fs), o(Vs, Ts, Ns1, Fs)) -->
    vertex_normal(N),
    {
        diff_list_append(Ns0, N, Ns1)
    }.

read_line(o(Vs, Ts, Ns, Fs0), o(Vs, Ts, Ns, Fs1)) -->
    face(F),
    {
        diff_list_append(Fs0, F, Fs1)
    }.

read_line(State, State) -->
    comment.

read_line(State, State) -->
    white_eol.

vertex(v(X, Y, Z, W)) -->
    "v",
    white_number(X),
    white_number(Y),
    white_number(Z),
    white_maybe_number(W, 1.0),
    white_eol,
    !.


texture_coordinate(vt(U, V, W)) -->
    "vt",
    white_number(U),
    white_number(V),
    white_maybe_number(W, 0.0),
    white_eol.


vertex_normal(vn(X, Y, Z)) -->
    "vn",
    white_number(X),
    white_number(Y),
    white_number(Z),
    white_eol.


face(f(V1, V2, V3)) -->
    "f",
    white_vertex(V1),
    white_vertex(V2),
    white_vertex(V3),
    white_eol.

face(f(V1, V2, V3)) -->
    "f",
    white_vertex_texture(V1),
    white_vertex_texture(V2),
    white_vertex_texture(V3),
    white_eol.

face(f(V1, V2, V3)) -->
    "f",
    white_vertex_normal(V1),
    white_vertex_normal(V2),
    white_vertex_normal(V3),
    white_eol.

face(f(V1, V2, V3)) -->
    "f",
    white_vertex_texture_normal(V1),
    white_vertex_texture_normal(V2),
    white_vertex_texture_normal(V3),
    white_eol.


white_vertex(V) -->
    white_integer(V).


white_vertex_texture(V/VT) -->
    white,
    whites,
    integer(V),
    "/",
    integer(VT).


white_vertex_normal(V//VN) -->
    white,
    whites,
    integer(V),
    "//",
    integer(VN).


white_vertex_texture_normal(V/VT/VN) -->
    white_vertex_texture(V/VT),
    "/",
    integer(VN).


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

diff_list_init(X-X).

diff_list_append(A-[E|AE], E, A-AE).

diff_list_close(X-[], X).






