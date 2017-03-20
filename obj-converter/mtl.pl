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


read_line(Current_Materials, New_Materials, _, Name) -->
	new_material(Name),
	{
	    default_material(New_Material),
	    put_dict(Name, Current_Materials, New_Material, New_Materials)
	}.

read_line(Current_Materials, New_Materials, Current_Material, Current_Material) -->
	ambient_colour(Ka),
	update_material_property(Current_Materials, Current_Material, ka, Ka, New_Materials).

read_line(Current_Materials, New_Materials, Current_Material, Current_Material) -->
	diffuse_colour(Kd),
	update_material_property(Current_Materials, Current_Material, kd, Kd, New_Materials).

read_line(Current_Materials, New_Materials, Current_Material, Current_Material) -->
	specular_colour(Ks),
	update_material_property(Current_Materials, Current_Material, ks, Ks, New_Materials).

read_line(Current_Materials, New_Materials, Current_Material, Current_Material) -->
	specular_exponent(Ns),
	update_material_property(Current_Materials, Current_Material, ns, Ns, New_Materials).

read_line(Current_Materials, New_Materials, Current_Material, Current_Material) -->
	reflectivity(Kr),
	update_material_property(Current_Materials, Current_Material, kr, Kr, New_Materials).

read_line(Current_Materials, New_Materials, Current_Material, Current_Material) -->
	ambient_colour_map(Map_Ka),
	update_material_property(Current_Materials, Current_Material, map_ka, Map_Ka, New_Materials).

read_line(Current_Materials, New_Materials, Current_Material, Current_Material) -->
	diffuse_colour_map(Map_Kd),
	update_material_property(Current_Materials, Current_Material, map_kd, Map_Kd, New_Materials).

read_line(Current_Materials, New_Materials, Current_Material, Current_Material) -->
	specular_colour_map(Map_Ks),
	update_material_property(Current_Materials, Current_Material, map_kd, Map_Ks, New_Materials).

read_line(Current_Materials, New_Materials, Current_Material, Current_Material) -->
	specular_exponent_map(Map_Ns),
	update_material_property(Current_Materials, Current_Material, map_kd, Map_Ns, New_Materials).

read_line(Current_Materials, New_Materials, Current_Material, Current_Material) -->
	reflectivity_map(Map_Kr),
	update_material_property(Current_Materials, Current_Material, map_kd, Map_Kr, New_Materials).

read_line(Materials, Materials, Material, Material) -->
	comment.

read_line(Materials, Materials, Material, Material) -->
	white_eol.

read_line(Materials, Materials, Material, Material) -->
	string_without("\r\n", Line_Codes),
	{
	    string_codes(Line, Line_Codes),
	    format("Unknown line ~w", [Line])
	}.

update_material_property(Current_Materials, Current_Material, Property_Name, Property_Value, New_Materials) :-
	get_dict(Current_Material, Current_Materials, Current_Material_Values),
	put_dict(Property_Name, Current_Material_Values, Property_Value, New_Material_Values),
	put_dict(Current_Material, Current_Materials, New_Material_Values, New_Materials).

update_material_property(Current_Materials, Current_Material, Property_Name, Property_Value, New_Materials, Codes, Codes) :-
	update_material_property(Current_Materials, Current_Material, Property_Name, Property_Value, New_Materials).

new_material(Name) -->
	"newmtl",
	white,
	whites,
	nonblanks(Name_Codes),
	{
	    atom_codes(Name, Name_Codes)
	},
	white_eol.


default_material(
    mtl{ka:Ka, kd:Kd, ks:Ks, ns:Ns, kr:Kr, map_ka:Map_Ka, map_kd:Map_Kd, map_ks:Map_Ks, map_ns:Map_Ns, map_kr:Map_Kr}) :-
	white(Ka),
	white(Kd),
	black(Ks),
	Ns = number(0),
	black(Kr),
	Map_Ka = file(""),
	Map_Kd = file(""),
	Map_Ks = file(""),
	Map_Ns = file(""),
	Map_Kr = file("").


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


reflectivity(Colour) -->
	k_constant("r", Colour).


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


reflectivity_map(File) -->
	k_map("r", File).


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
