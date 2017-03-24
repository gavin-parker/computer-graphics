#pragma once

#include <omp.h>

#include "camera.h"
#include "cube.h"
#include "cube.h"
#include "light.h"
#include "pointlight.h"
#include "scene.h"
#include "sdlscreen.h"
#include "spherelight.h"

using glm::ivec2;

class LightingEngine {
public:
  const vector<Triangle> &triangles;
  const Light &light;

  int countedSamples = 1;

  LightingEngine() = delete;

  LightingEngine(const LightingEngine &other) = delete;

  LightingEngine(const vector<Triangle> &triangles, const Light &light)
      : triangles(triangles), light(light){};

  virtual vec3 calculateLight(Ray &ray, ivec2 pixel = ivec2(0, 0)) = 0;
};
