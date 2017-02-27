:- module(mtl, [
	      mtl_file/2,
	      mtl_file/3,
	      mtl//1,
	      mtl//2,
	      initial_materials/1
	  ]).

:- use_module(library(dcg/basics)).

:- use_module(util).

mtl_file(Materials, File) :-
	phrase_from_file(mtl(Materials), File).

mtl_file(Current_Materials, New_Materials, File) :-
	phrase_from_file(mtl(Current_Materials, New_Materials), File).


mtl(Materials, Codes, End) :-
	initial_materials(Initial_Materials),
	mtl(Initial_Materials, Materials, Codes, End).

mtl(Current_Materials, New_Materials, Codes, End) :-
	initial_material(Initial_Material),
	read_lines(Current_Materials, New_Materials, Initial_Material, _, Codes, End).


initial_materials(materials{}).


initial_material(null).


read_lines(Final_Materials, Final_Materials, Final_Material, Final_Material) -->
	eos.

read_lines(Current_Materials, Final_Materials, Current_Material, Final_Material) -->
	read_line(Current_Materials, New_Materials, Current_Material, New_Material),
	!,
	read_lines(New_Materials, Final_Materials, New_Material, Final_Material).


read_line(Current_Materials, New_Materials, _, New_Material) -->
	new_material(Name),
	{
	    default_material(New_Material),
	    put_dict(Name, Current_Materials, New_Material, New_Materials)
	}.

read_line(Materials, Materials, mtl(_Ka, Kd, Ks, Ns, Map_Ka, Map_Kd, Map_Ks, Map_Ns), mtl(Ka, Kd, Ks, Ns, Map_Ka, Map_Kd, Map_Ks, Map_Ns)) -->
	ambient_colour(Ka).

read_line(Materials, Materials, mtl(Ka, _Kd, Ks, Ns, Map_Ka, Map_Kd, Map_Ks, Map_Ns), mtl(Ka, Kd, Ks, Ns, Map_Ka, Map_Kd, Map_Ks, Map_Ns)) -->
	diffuse_colour(Kd).

read_line(Materials, Materials, mtl(Ka, Kd, _Ks, Ns, Map_Ka, Map_Kd, Map_Ks, Map_Ns), mtl(Ka, Kd, Ks, Ns, Map_Ka, Map_Kd, Map_Ks, Map_Ns)) -->
	specular_colour(Ks).

read_line(Materials, Materials, mtl(Ka, Kd, Ks, _Ns, Map_Ka, Map_Kd, Map_Ks, Map_Ns), mtl(Ka, Kd, Ks, Ns, Map_Ka, Map_Kd, Map_Ks, Map_Ns)) -->
	specular_exponent(Ns).

read_line(Materials, Materials, mtl(Ka, Kd, Ks, Ns, _Map_Ka, Map_Kd, Map_Ks, Map_Ns), mtl(Ka, Kd, Ks, Ns, Map_Ka, Map_Kd, Map_Ks, Map_Ns)) -->
	ambient_colour_map(Map_Ka).

read_line(Materials, Materials, mtl(Ka, Kd, Ks, Ns, Map_Ka, _Map_Kd, Map_Ks, Map_Ns), mtl(Ka, Kd, Ks, Ns, Map_Ka, Map_Kd, Map_Ks, Map_Ns)) -->
	diffuse_colour_map(Map_Kd).

read_line(Materials, Materials, mtl(Ka, Kd, Ks, Ns, Map_Ka, Map_Kd, _Map_Ks, Map_Ns), mtl(Ka, Kd, Ks, Ns, Map_Ka, Map_Kd, Map_Ks, Map_Ns)) -->
	specular_colour_map(Map_Ks).

read_line(Materials, Materials, mtl(Ka, Kd, Ks, Ns, Map_Ka, Map_Kd, Map_Ks, _Map_Ns), mtl(Ka, Kd, Ks, Ns, Map_Ka, Map_Kd, Map_Ks, Map_Ns)) -->
	specular_exponent_map(Map_Ns).

read_line(Materials, Materials, Material, Material) -->
	comment.

read_line(Materials, Materials, Material, Material) -->
	white_eol.

read_line(Materials, Materials, Material, Material) -->
	string_without("\r\n", Line_Codes),
	{
	    string_codes(Line, Line_Codes),
	    format("Unknown line ~w", [Line]),
	    fail
	}.


new_material(Name) -->
	"newmtl",
	white,
	whites,
	nonblanks(Name_Codes),
	{
	    atom_codes(Name, Name_Codes)
	},
	white_eol.


default_material(mtl(Ka, Kd, Ks, Ns, Map_Ka, Map_Kd, Map_Ks, Map_Ns)) :-
	white(Ka),
	white(Kd),
	black(Ks),
	Ns = number(0),
	Map_Ka = file(""),
	Map_Kd = file(""),
	Map_Ks = file(""),
	Map_Ns = file("").


black(rgb(0.0, 0.0, 0.0)).


white(rgb(1.0, 1.0, 1.0)).


ambient_colour(Colour) -->
	k_constant("a", Colour).


diffuse_colour(Colour) -->
	k_constant("d", Colour).


specular_colour(Colour) -->
	k_constant("s", Colour).


specular_exponent(number(Exponent)) -->
	whites,
	"Ns",
	white_number(Exponent),
	white_eol.


k_constant(Name, rgb(R, G, B)) -->
	whites,
	"K",
	Name,
	white_number(R),
	white_number(G),
	white_number(B),
	white_eol.


ambient_colour_map(File) -->
	k_map("a", File).


diffuse_colour_map(File) -->
	k_map("d", File).


specular_colour_map(File) -->
	k_map("s", File).


specular_exponent_map(File) -->
	white,
	"map_Ns",
	white,
	whites,
	nonblanks(File_Codes),
	{
	    atom_codes(File, File_Codes)
	},
	white_eol.


k_map(Name, file(File)) -->
	whites,
	"map_K",
	Name,
	white,
	whites,
	nonblanks(File_Codes),
	{
	    atom_codes(File, File_Codes)
	},
	white_eol.
