#pragma once

#include "lightingengine.h"

class StandardLighting final : public LightingEngine {
private:
  const BoundingVolume &boundingVolume;

public:
  StandardLighting(const Scene &scene);
  vec3 calculateLight(Ray &cameraRay, ivec2 pixel = ivec2(0, 0)) override;
};
