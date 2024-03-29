#pragma once

#include "bvh.h"
#include "cube.h"
#include "lightingengine.h"

struct Scene {
  Light &light;
  const Ptr_Triangles &triangles;
  const BoundingVolume &volume;
  const Cube &bounds;
};
