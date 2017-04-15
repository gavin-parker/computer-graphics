#pragma once

#include "cube.h"

class HasSpeed {
public:
  const float speed;

  HasSpeed(float speed);

  HasSpeed(const Cube &bounds, float timePeriod);

  static float calculateSpeed(const Cube &bounds, float timePeriod);
};
