#include "hasspeed.h"

HasSpeed::HasSpeed(float speed) : speed(speed) {}

HasSpeed::HasSpeed(const Cube &bounds, float timePeriod)
    : speed(calculateSpeed(bounds, timePeriod)) {}

float HasSpeed::calculateSpeed(const Cube &bounds, float timePeriod) {
  float width = bounds.b.x - bounds.a.x;

  return width / timePeriod;
}
