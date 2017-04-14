#pragma once

#include <glm/glm.hpp>

using glm::vec3;

class Pixel {
public:
  int x, y;
  float depth;

  vec3 position;

  Pixel();

  Pixel(int x);

  Pixel(int x, int y, float depth, vec3 position);
};
