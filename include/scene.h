#pragma once

#include "bvh.h"
#include "lightingengine.h"

struct Scene {
  Light &light;
  const vector<Triangle> &triangles;
  const BoundingVolume &volume;
};
