#pragma once

#define _USE_MATH_DEFINES

#include "light.h"
#include <cmath>

class SphereLight final : public Light {
private:
  const float velocity = 1.0f;

  const float radius = 1.0f;

public:
  SphereLight(vec3 position, vec3 colour, float power, float radius, int res);

  bool update(float dt) override;

  vector<Ray> calculateRays(vec3 target) const override;
};
