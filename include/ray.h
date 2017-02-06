#pragma once

#include <glm/glm.hpp>

struct Ray;

#include "triangle.h"

using glm::vec3;
using glm::mat3;

struct Ray {
  vec3 position;
  vec3 direction;

  Triangle const *collision;
  vec3 collisionLocation;
  vec2 collisionUVLocation;
  float length;
};
