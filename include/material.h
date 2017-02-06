#pragma once
#include <glm/glm.hpp>
#include <vector>

using glm::vec2;
using std::vector;

class Material {
public:
  const vector<unsigned char> texture;
  const int size;
  const vec2 tiling;
  Material(vector<unsigned char> &texture, int size, vec2 tiling = vec2(1, 1));
};
