#pragma once

#include "gi.h"

using glm::ivec2;

class ConvergentGlobalIllumination : public LightingEngine {
private:
  vec3 environment = vec3(1, 1, 1) * 0.2f;
  const shared_ptr<const Cube> boundingBox;
  GlobalIllumination gi;
  vector<vec3> image;
  int width;
  int height;

protected:
public:
  ConvergentGlobalIllumination();
  ConvergentGlobalIllumination(const Scene &scene, int sampleCount, int width,
                               int height);

  vec3 calculateLight(Ray &ray, ivec2 pixel = ivec2(0, 0)) override;
};
