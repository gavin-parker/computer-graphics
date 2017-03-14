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
  const vec3 normal;
  const vec3 colour;
  const shared_ptr<const Material> mat;
  const bool refractive;
  const bool reflective;

  Triangle(vec3 v0, vec3 v1, vec3 v2, vec2 vt0, vec2 vt1, vec2 vt2, vec3 colour,
           const shared_ptr<const Material> mat, bool refractive = false, bool reflective = false);

  bool calculateIntersection(Ray &ray) const;


  vec3 getColour(vec2 uv) const;

  vec3 getColour(vec3 bary) const;


  vec3 getPixelColour(vec2 uv) const;
};
