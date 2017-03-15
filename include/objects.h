#pragma once

#include "object.h"

class Box : public Object {
public:
  Box();

  vector<Triangle> allTriangles();

  BoundingVolume createBoundingVolume();
};
