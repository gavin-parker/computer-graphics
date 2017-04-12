#pragma once

#include "light.h"

class PointLight : public Light {
public:
  const float speed = 100.0f;

  PointLight(vec3 position, vec3 colour, float power);

  bool update(float dt) override;

  vector<Ray> calculateRays(vec3 target) const override;
};
