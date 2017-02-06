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
  const vec3 v0, e1, e2;
  const vec2 vt0, et1, et2;
  const vec3 normal;
  const vec3 colour;
  const Material mat;
  Triangle(vec3 v0, vec3 v1, vec3 v2, vec2 vt0, vec2 vt1, vec2 vt2, vec3 colour,
           Material mat);

  bool calculateIntection(Ray &ray) const;

  vec3 getColour(vec2 uv) const;
};
