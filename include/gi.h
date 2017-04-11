#pragma once

#include "lightingengine.h"

class GlobalIllumination : public LightingEngine {
private:
  vec3 trace(Ray &ray, int bounces);

  bool anyIntersection(Ray &ray, Ray &surface);
  vec3 getLightHere(Ray &ray, vec3 reflection);
  int sampleCount = 10;
  int total_bounces = 3;
  vec3 environment = vec3(1, 1, 1) * 0.2f;

  const BoundingVolume &boundingVolume;
  vec3 reflectedRatio(vec3 lightIn, vec3 reflection, vec3 normal, const Ptr_Material mat) const;
public:
  GlobalIllumination();
  GlobalIllumination(const Scene &scene, int sampleCount);

  vec3 calculateLight(Ray &ray, ivec2 pixel = ivec2(0, 0)) override;
};
