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
  Material();

  bool loadPNG(string filename);

  vec3 getColour(vec2 uv) const;
};
