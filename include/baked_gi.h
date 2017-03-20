#pragma once
#include "gi.h"
#include "lightingengine.h"

class BakedGI : public LightingEngine {
private:
  vec3 trace(Ray &ray, int bounces);

  bool anyIntersection(Ray &ray, Ray &surface);
  int sampleCount = 10;
  int total_bounces = 3;
  vec3 environment = vec3(1, 1, 1) * 0.2f;
  const shared_ptr<BoundingVolume> boundingVolume;
  void constructImage();
  vector<vector<vec3>> image;
  int resolution;

protected:
public:
  BakedGI();
  BakedGI(const shared_ptr<Scene> scene, int sampleCount, int resolution);

  vec3 calculateLight(Ray &ray, ivec2 pixel = ivec2(0, 0)) override;
};
