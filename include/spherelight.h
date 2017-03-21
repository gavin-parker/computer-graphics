#pragma once
#include "light.h"

class SphereLight : public Light {
private:
  const float velocity = 1.0f;

  const float radius = 1.0f;

public:
  SphereLight(vec3 position, vec3 color, float power, float radius, int res);

  bool update(float dt) override;

  vector<Ray> calculateRays(vec3 target) const override;

  vec3 directLight(const Ray &ray) const override;

  vec3 vertexLight(Vertex v) const override;
};
