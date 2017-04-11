#pragma once

#include "material.h"
#include <algorithm>
#include <limits>
#include <memory>
#include <vector>

using std::shared_ptr;
using std::vector;
using std::numeric_limits;

class Cube;

#include "ray.h"

class Cube {
public:
  vec3 a, b;

  Cube();

  Cube(vec3 a, vec3 b);

  Cube(const Ptr_Triangles &triangles);

  bool calculateIntersection(Ray &ray) const;
};
