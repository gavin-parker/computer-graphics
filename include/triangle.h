#pragma once

#include "material.h"
#include <algorithm>
#include <memory>
#include <vector>

using std::shared_ptr;
using std::vector;

class Triangle;

#include "ray.h"

class Triangle {
public:
  const vec3 v0, v1, v2, e1, e2;
  const vec2 vt0, vt1, vt2, et1, et2;
  const vec3 vn0, vn1, vn2, en1, en2, normal;
  const Material &mat;

  Triangle(vec3 v0, vec3 v1, vec3 v2, vec2 vt0, vec2 vt1, vec2 vt2, vec3 vn0,
           vec3 vn1, vec3 vn2, const Material &mat);

  bool calculateIntersection(Ray &ray) const;

  vec3 colour(vec2 uv, vec3 ambientLight) const;

  vec3 colour(vec2 uv, vec3 ambientLight, vec3 lightDirection,
              vec3 diffuseLight) const;
};
