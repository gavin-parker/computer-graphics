#include "material.h"

Material::Material(vector<unsigned char> &texture, int size, vec2 tiling)
    : texture(texture), size(size), tiling(tiling) {}
