#pragma once

#include "lightingengine.h"

class FlatLighting final : public LightingEngine {

public:
  FlatLighting(Scene &scene);
  FlatLighting();

  vec3 calculateLight(Ray &cameraRay, ivec2 pixel = ivec2(0, 0)) override;
};
