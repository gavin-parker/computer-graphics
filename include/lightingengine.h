#pragma once

#include "camera.h"
#include "cube.h"
#include "cube.h"
#include "light.h"
#include "pointlight.h"
#include "scene.h"
#include "sdlscreen.h"
#include "spherelight.h"
#include <omp.h>

using glm::ivec2;

class LightingEngine {
public:
  const Ptr_Triangles &triangles;
  const Light &light;

  int countedSamples = 1;

  LightingEngine() = delete;

  LightingEngine(const LightingEngine &other) = delete;

  LightingEngine(const Ptr_Triangles &triangles, const Light &light);

  virtual vec3 calculateLight(Ray &ray, ivec2 pixel = ivec2(0, 0)) = 0;
};
