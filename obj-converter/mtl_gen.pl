:- module(mtl_gen, [
              materials_array//1
          ]).

:- use_module(library(dcg/basics)).
:- use_module(library(lists)).

:- use_module(values).

materials_array(Materials) -->
    "MaterialTemplate materials[] = {\n",
    {
        dict_pairs(Materials, _, Pairs)
    },
    materials(Pairs),
    "};\n".

materials([]) -->
    "",
    !.

materials([(Name-Material)|Materials]) -->
          "MaterialTemplate(",
          {
              compound_name_arguments(Material, _, Material_Values)
          },
          comma_separated_values([string(Name)|Material_Values]),
          ")\n",
          materials(Materials).
