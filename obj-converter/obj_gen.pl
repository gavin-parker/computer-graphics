:- module(obj_gen, [
              generate_object//2
          ]).


:- use_module(library(lists)).

:- use_module(mtl_gen).
:- use_module(values).

generate_object(Object_Name, o(V, T, N, G, M)) -->
    Object_Name,
    "::",
    Object_Name,
    "(){\n",
    generate_materials(M),
    generate_groups(V, T, N, G),
    "}\n".


generate_groups(_V, _T, _N, []) -->
    "",
    !.

generate_groups(V, T, N, [Group|Groups]) -->
    generate_group(V, T, N, Group),
    !,
    generate_groups(V, T, N, Groups).


generate_group(V, T, N, Name-Faces) -->
    generate_faces(Name, V, T, N, Faces).

generate_faces(_G, _V, _T, _N, []) -->
    "".

generate_faces(G, V, T, N, [Face|Faces]) -->
    generate_face(G, V, T, N, Face),
    !,
    generate_faces(G, V, T, N, Faces).


generate_face(Group, Positions, Texture_Coordinates, Normals, f(Vertex0, Vertex1, Vertex2, Material)) -->
    {
        vertex_info(Positions, Texture_Coordinates, Normals, Vertex0, V0, VT0, VN0),
        vertex_info(Positions, Texture_Coordinates, Normals, Vertex1, V1, VT1, VN1),
        vertex_info(Positions, Texture_Coordinates, Normals, Vertex2, V2, VT2, VN2),
        format("triangle: ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w ~w\n", [Group, V0, V1, V2, VT0, VT1, VT2, VN0, VN1, VN2, Material])
    },
    "AddFace(",
    comma_separated_values([string(Group), V0, V1, V2, VT0, VT1, VT2, VN0, VN1, VN2, string(Material)]),
    ");\n".


vertex_info(Positions, _Texture_Coordinates, _Normals, Position_Index, Position, Texture_Coordinate, Normal) :-
    integer(Position_Index),
    nth0(Position_Index, Positions, Position),
    default_texture_coordinate(Texture_Coordinate),
    default_normal(Normal).

vertex_info(Positions, Texture_Coordinates, _Normals, Position_Index/Texture_Index, Position, Texture_Coordinate, Normal) :-
    integer(Position_Index),
    integer(Texture_Index),
    nth1(Position_Index, Positions, Position),
    nth1(Texture_Index, Texture_Coordinates, Texture_Coordinate),
    default_normal(Normal).

vertex_info(Positions, _Texture_Coordinates, Normals, Position_Index//Normal_Index, Position, Texture_Coordinate, Normal) :-
    integer(Position_Index),
    integer(Normal_Index),
    nth1(Position_Index, Positions, Position),
    default_texture_coordinate(Texture_Coordinate),
    nth1(Normal_Index, Normals, Normal).

vertex_info(Positions, Texture_Coordinates, Normals, Position_Index/Texture_Index/Normal_Index, Position, Texture_Coordinate, Normal) :-
    integer(Position_Index),
    integer(Texture_Index),
    integer(Normal_Index),
    nth1(Position_Index, Positions, Position),
    nth1(Texture_Index, Texture_Coordinates, Texture_Coordinate),
    nth1(Normal_Index, Normals, Normal).


default_texture_coordinate(vt(0.0, 0.0)).


default_normal(vn(1.0, 1.0, 1.0, 1.0)).











