#pragma once
#include "lightingengine.h"



class GlobalIllumination : public LightingEngine {
private:


  vec3 trace(Ray ray, int bounces);

  bool anyIntersection(Ray &ray, Ray &surface);
  int sampleCount = 10;
  int total_bounces = 3;
  vec3 environment = vec3(1, 1, 1)*0.2f;
  const shared_ptr<BoundingVolume> boundingVolume;

protected:
public:
  GlobalIllumination();
  GlobalIllumination(const shared_ptr<Scene> scene, int sampleCount);

  vec3 calculateLight(Ray ray, ivec2 pixel = ivec2(0, 0)) override;
};
