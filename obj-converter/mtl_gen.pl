:- module(mtl_gen, [
              generate_materials//1
          ]).

:- use_module(values).

generate_materials(Materials, Codes, Rest) :-
    dict_pairs(Materials, _, Pairs),
    materials(Pairs, Codes, Rest).

materials([]) -->
    "",
    !.

materials([(Name-Material)|Materials]) -->
          "AddMaterial(",
          {
              compound_name_arguments(Material, _, Material_Values)
          },
          comma_separated_values([string(Name)|Material_Values]),
          ");\n",
          materials(Materials).
