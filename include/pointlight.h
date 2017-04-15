#pragma once

#include "light.h"

class PointLight final : public Light {
public:
  PointLight(vec3 position, const Cube &bounds, float timePeriod, vec3 colour,
             float power);

  vector<Ray> calculateRays(vec3 target) const override;
};
