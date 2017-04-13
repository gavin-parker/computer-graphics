#pragma once

#include <omp.h>

#include "light.h"
#include "pointlight.h"
#include "scene.h"
#include "sdlscreen.h"
#include "spherelight.h"

using glm::ivec2;

class LightingEngine {
public:
  const vec3 ambientLight = vec3(0.1f, 0.1f, 0.1f);

  const Ptr_Triangles &triangles;
  const Light &light;

  int countedSamples = 1;

  LightingEngine() = delete;

  LightingEngine(const LightingEngine &other) = delete;

  LightingEngine(const Ptr_Triangles &triangles, const Light &light);

  virtual ~LightingEngine();

  virtual vec3 calculateLight(Ray &cameraRay, ivec2 pixel = ivec2(0, 0)) = 0;
};
