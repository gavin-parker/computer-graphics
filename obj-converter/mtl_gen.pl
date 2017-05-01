:- module(mtl_gen, [
              generate_materials//1
          ]).

:- use_module(values).

generate_materials(Materials) -->
    "Materials: ",
    {
        dict_pairs(Materials, _, Pairs)
    },
    space_separated_values([list_count(Pairs), eol]),
    materials(Pairs).

materials([]) -->
    "",
    !.

materials([(Name-Material)|Materials]) -->
          "Material: ",
          {
              Material = M
          },
          space_separated_values([string(Name), M.ka, M.kd, M.ks, M.ns, M.map_ka, M.map_kd, M.map_ks, M.map_ns, M.map_norm, M.mirror, M.refractive, eol]),
          !,
          materials(Materials).
