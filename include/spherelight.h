#pragma once

#define _USE_MATH_DEFINES

#include "light.h"
#include <cmath>

class SphereLight final : public Light {
private:
  const float radius = 1.0f;

public:
  SphereLight(vec3 position, const Cube &bounds, float timePeriod, vec3 colour,
              float power, float radius, int res);

  vector<Ray> calculateRays(vec3 target) const override;
};
