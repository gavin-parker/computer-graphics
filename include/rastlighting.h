#pragma once

#include "lightingengine.h"

class RastLighting : public LightingEngine {
private:
  bool ClosestIntersection(Ray &ray);
  bool anyIntersection(Ray &ray, Ray &surface);
  int sampleCount = 5;
  vec3 ambientLight = vec3(0.1f, 0.1f, 0.1f);
  const BoundingVolume &boundingVolume;
  int lightMapResolution;
  vector<float> depthMap;
  void fillShadowMap();

protected:
public:
  RastLighting(const Scene &scene, int lightMapResolution = 2000);
  RastLighting();
  virtual vec3 calculateLight(Ray &ray, ivec2 pixel = ivec2(0, 0)) override;
};
#pragma once
