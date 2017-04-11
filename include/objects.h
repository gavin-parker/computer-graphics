#pragma once

#include "object.h"

class Box : public Object {
public:
  Box();
  Box(BRDF &brdf);

  BoundingVolume createBoundingVolume();
};

class Teapot : public Object {
public:
  Teapot();

  BoundingVolume createBoundingVolume();
};
