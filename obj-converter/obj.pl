:- module(obj, [
              obj_file/2,
              obj//1
          ]).

:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

:- use_module(diff_list).
:- use_module(mtl).
:- use_module(util).


obj_file(Object, File) :-
    phrase_from_file(obj(Object), File).


obj(Object, Codes, End) :-
    initial_state(Initial_State),
    read_lines(Initial_State, Final_State, Codes, End),
    final_state(Final_State, Object).


initial_state(o(V, T, N, [''], groups{}, '', Materials)) :-
    diff_list_init(V),
    diff_list_init(T),
    diff_list_init(N),
    initial_materials(Materials).


final_state(o(VsL, TsL, NsL, _Current_Group, Gs, _Current_Material, Materials), o(Vs, Ts, Ns, GPs, Materials)) :-
    diff_list_close(VsL, Vs),
    diff_list_close(TsL, Ts),
    diff_list_close(NsL, Ns),
    dict_pairs(Gs, _, GLPs),
    close_dict_pairs(GLPs, GPs).


close_dict_pairs([Name-Diff_List|Diff_Pairs], [Name-List|Pairs]) :-
    diff_list_close(Diff_List, List),
    close_dict_pairs(Diff_Pairs, Pairs).

close_dict_pairs([], []).


read_lines(State, State) -->
    eos,
    !.

read_lines(Initial_State, Final_State) -->
    read_line(Initial_State, Intermediate_State),
    !,
    read_lines(Intermediate_State, Final_State).


read_line(o(Vs0, Ts, Ns, G_Current, G_All, M_Current, M_All), o(Vs1, Ts, Ns, G_Current, G_All, M_Current, M_All)) -->
    vertex(V),
    {
	convert_to_vec3(V, V3),
        format("vertex: ~w\n", V3),
        diff_list_append(Vs0, V3, Vs1)
    }.

read_line(o(Vs, Ts0, Ns, G_Current, G_All, M_Current, M_All), o(Vs, Ts1, Ns, G_Current, G_All, M_Current, M_All)) -->
    texture_coordinate(T),
    {
        format("texture coord: ~w\n", T),
        diff_list_append(Ts0, T, Ts1)
    }.

read_line(o(Vs, Ts, Ns0, G_Current, G_All, M_Current, M_All), o(Vs, Ts, Ns1, G_Current, G_All, M_Current, M_All)) -->
    vertex_normal(N),
    {
	convert_to_vec3(N, N3),
        format("normal: ~w\n", N3),
        diff_list_append(Ns0, N3, Ns1)
    }.

read_line(o(Vs, Ts, Ns, G_Current, G_All0, M_Current, M_All), o(Vs, Ts, Ns, G_Current, G_All1, M_Current, M_All)) -->
    face(F, M_Current),
    {
        format("face: ~w\n", F),
        add_face(F, G_Current, G_All0, G_All1)
    }.

read_line(o(Vs, Ts, Ns, _, G_All, M_Current, M_All), o(Vs, Ts, Ns, G_Current, G_All, M_Current, M_All)) -->
    group(G_Current).

read_line(o(Vs, Ts, Ns, G_Current, G_All, M_Current, M_All0), o(Vs, Ts, Ns, G_Current, G_All, M_Current, M_All1)) -->
	material_library(File),
	{
	    mtl_file(M_All0, M_All1, File)
	}.

read_line(o(Vs, Ts, Ns, G_Current, G_All, _M_Current, M_All), o(Vs, Ts, Ns, G_Current, G_All, M_Current, M_All)) -->
	use_material(M_Current).

read_line(State, State) -->
    comment.

read_line(State, State) -->
    white_eol.

read_line(State, State) -->
	string_without("\r\n", Line_Codes),
	{
	    string_codes(Line, Line_Codes),
	    format("Unknown line ~w", [Line]),
	    fail
	}.


convert_to_vec3(Input, Output) :-
	compound_name_arguments(Input, Name, [X, Y]),
	compound_name_arguments(Output, Name, [X, Y, 0.0]).

convert_to_vec3(Input, Output) :-
	compound_name_arguments(Input, Name, [X, Y, Z]),
	compound_name_arguments(Output, Name, [X, Y, Z]).

convert_to_vec3(Input, Output) :-
	compound_name_arguments(Input, Name, [X, Y, Z, W]),
	XW is X / W,
	YW is Y / W,
	ZW is Z / W,
	compound_name_arguments(Output, Name, [XW, YW, ZW]).


vertex(v(X, Y, Z, W)) -->
    "v",
    white_number(X),
    white_number(Y),
    white_number(Z),
    white_maybe_number(W, 1.0),
    white_eol,
    !.


texture_coordinate(vt(U, V)) -->
    "vt",
    white_number(U),
    white_number(V),
    white_eol.


vertex_normal(vn(X, Y, Z)) -->
    "vn",
    white_number(X),
    white_number(Y),
    white_number(Z),
    white_eol.


face(f(V1, V2, V3, M_Current), M_Current) -->
    "f",
    white_vertex(V1),
    white_vertex(V2),
    white_vertex(V3),
    white_eol.

face(f(V1, V2, V3, M_Current), M_Current) -->
    "f",
    white_vertex_texture(V1),
    white_vertex_texture(V2),
    white_vertex_texture(V3),
    white_eol.

face(f(V1, V2, V3, M_Current), M_Current) -->
    "f",
    white_vertex_normal(V1),
    white_vertex_normal(V2),
    white_vertex_normal(V3),
    white_eol.

face(f(V1, V2, V3, M_Current), M_Current) -->
    "f",
    white_vertex_texture_normal(V1),
    white_vertex_texture_normal(V2),
    white_vertex_texture_normal(V3),
    white_eol.


add_face(_, [], G_All, G_All).

add_face(Face, [Group|Groups], G_All0, G_All2) :-
    (   get_dict(Group, G_All0, Current_Faces)
    ->  true
    ;   diff_list_init(Current_Faces)),
    diff_list_append(Current_Faces, Face, New_Faces),
    put_dict(Group, G_All0, New_Faces, G_All1),
    add_face(Face, Groups, G_All1, G_All2).


group(Groups) -->
    "g",
    group_tail(Groups).


group_tail([]) -->
    white_eol.

group_tail([Group|Groups]) -->
    white,
    whites,
    nonblanks(Group_Codes),
    {
        atom_codes(Group, Group_Codes)
    },
    group_tail(Groups).


material_library(File) -->
	whites,
	"mtllib",
	whites,
	nonblanks(File_Codes),
	{
	    atom_codes(File, File_Codes)
	},
	white_eol.


use_material(Material) -->
	whites,
	"usemtl",
	whites,
	nonblanks(Material_Codes),
	{
	    atom_codes(Material, Material_Codes)
	},
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





