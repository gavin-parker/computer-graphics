#pragma once

#include <glm/glm.hpp>

using glm::vec3;
using glm::mat3;

struct Ray;

#include "triangle.h"

struct Ray {
  vec3 position;
  vec3 direction;

  Triangle const *collision;
  vec2 uv;
  float length;
};
