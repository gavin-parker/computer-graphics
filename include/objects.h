#pragma once

#include "object.h"

class Box : public Object {
public:
  Box();

  BoundingVolume createBoundingVolume();
};
