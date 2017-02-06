#pragma once

#include "material.h"
#include <algorithm>
#include <glm/glm.hpp>
#include <iostream>
#include <vector>

using glm::vec3;
using glm::vec2;
using glm::mat3x2;
using std::vector;

class Triangle;

#include "ray.h"

class Triangle {
public:
  const vec3 v0;
  const vec3 e1;
  const vec3 e2;
  const vec3 normal;
  const vec3 color;
  const Material mat;
  const mat3x2 colorMap;
  Triangle(vec3 v0, vec3 v1, vec3 v2, vec3 color, Material &mat,
           mat3x2 colorMap);

  bool calculateIntection(Ray &ray) const;

  vec3 getColor(float u, float v) const;
};
