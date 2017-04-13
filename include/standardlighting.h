#pragma once

#include "lightingengine.h"

class StandardLighting final : public LightingEngine {
private:
  bool ClosestIntersection(Ray &ray);
  bool anyIntersection(Ray &ray, Ray &surface);
  int sampleCount = 5;
  vec3 ambientLight = vec3(0.1f, 0.1f, 0.1f);
  const BoundingVolume &boundingVolume;

public:
  StandardLighting(const Scene &scene);
  vec3 calculateLight(Ray &cameraRay, ivec2 pixel = ivec2(0, 0)) override;
};
