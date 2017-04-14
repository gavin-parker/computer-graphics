#pragma once

#include "lightingengine.h"

class RastLighting final : public LightingEngine {
public:
  RastLighting(const Scene &scene);
  vec3 calculateLight(Ray &cameraRay, ivec2 pixel = ivec2(0, 0)) override;
};
