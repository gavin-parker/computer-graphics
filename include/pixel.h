#pragma once
#include <glm/glm.hpp>

using glm::vec3;
class Pixel {
public:
  int x, y;
  float depth;
  vec3 illumination;

  Pixel() : x(0), y(0), depth(0.0f), illumination(vec3(0, 0, 0)) {}
  Pixel(int x, int y, float depth)
      : x(x), y(y), depth(depth), illumination(vec3(0, 0, 0)) {}
  Pixel(int x, int y, float depth, vec3 illumination)
      : x(x), y(y), depth(depth), illumination(illumination) {}
};
