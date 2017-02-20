:- module(mtl, [
	      mtl//1
	  ]).

:- use_module(library(dcg/basics)).

:- use_module(util).

mtl(Materials, Codes, End) :-
	initial_state(Initial_Materials, Initial_Material),
	read_lines(Initial_Materials, Materials, Initial_Material, _, Codes, End).


initial_state(mtl{}, null).

read_lines(Final_Materials, Final_Materials, Final_Material, Final_Material) -->
	eos.

read_lines(Current_Materials, Final_Materials, Current_Material, Final_Material) -->
	read_line(Current_Materials, New_Materials, Current_Material, New_Material),
	read_line(New_Materials, Final_Materials, New_Material, Final_Material).


read_line(Current_Materials, New_Materials, _, New_Material) -->
	new_material(Name),
	{
	    default_material(New_Material),
	    put_dict(Name, Current_Materials, New_Material, New_Materials)
	}.

read_line(Materials, Materials, mtl(_, Diffuse, Specular), mtl(Ambient, Diffuse, Specular)) -->
	ambient_colour(Ambient).

read_line(Materials, Materials, mtl(_, Diffuse, Specular), mtl(Ambient, Diffuse, Specular)) -->
	ambient_colour_map(Ambient).

read_line(Materials, Materials, mtl(Ambient, _, Specular), mtl(Ambient, Diffuse, Specular)) -->
	ambient_colour(Diffuse).

read_line(Materials, Materials, mtl(Ambient, _, Specular), mtl(Ambient, Diffuse, Specular)) -->
	ambient_colour_map(Diffuse).



new_material(Name) -->
	"newmtl",
	white,
	whites,
	nonblanks(Name_Codes),
	{
	    atom_codes(Name, Name_Codes)
	},
	white_eol.


default_material(mtl(Ambient, Diffuse, Specular, Specular_Highlight)) :-
	black(Ambient),
	black(Diffuse),
	black(Specular),
	Specular_Highlight = 0.


black(rgb(0.0, 0.0, 0.0)).


ambient_colour(Colour) -->
	k_constant("a", Colour).


diffuse_colour(Colour) -->
	k_constant("d", Colour).


specular_colour(Colour, Exponent) -->
	k_constant("s", Colour),
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


specular_colour_map(Colour_File, file(Highlight_File)) -->
	k_map("s", Colour_File),
	whites,
	"map_Ns",
	white,
	whites,
	nonblanks(Highlight_File_Codes),
	{
	    atom_codes(Highlight_File, Highlight_File_Codes)
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
