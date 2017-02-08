#pragma once

#include <glm/glm.hpp>
#include <iostream>

#include "lodepng.h"
using glm::vec2;
using glm::vec3;
using std::cout;
using std::endl;
using std::string;
using std::vector;

class Material {
private:
  vector<unsigned char> texture;
  unsigned width, height;

public:
  int specular_falloff = 100;
  float specularity = 0.5f;
  float diffuse = 0.5f;
  Material();

  bool loadPNG(string filename);

  vec3 getColour(vec2 uv) const;

  vec3 phong(vec3 v, vec3 l, vec3 n) const;
};
