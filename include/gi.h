#pragma once

#include "lightingengine.h"

class GlobalIllumination final : public LightingEngine {
private:
  vec3 trace(Ray &ray, int bounces);


  int sampleCount = 10;
  int total_bounces = 3;
  vec3 environment = vec3(1, 1, 1) * 0.1f;

  const BoundingVolume &boundingVolume;

public:
  GlobalIllumination();
  GlobalIllumination(const Scene &scene, int sampleCount);

  vec3 calculateLight(Ray &ray, ivec2 pixel = ivec2(0, 0)) override;
};
