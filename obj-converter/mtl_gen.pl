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
              Material = M
          },
          comma_separated_values([string(Name), M.ka, M.kd, M.ks, M.ns, M.map_ka, M.map_kd, M.map_ks, M.map_ks, M.mirror]),
          ");\n",
          materials(Materials).
