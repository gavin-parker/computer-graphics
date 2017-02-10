#pragma once

#include <limits>
#include <omp.h>

#include "camera.h"
#include "pointlight.h"
#include "scene.h"
#include "sdlscreen.h"
#include "testmodel.h"
#include "cube.h"
using glm::ivec2;
using std::numeric_limits;

class LightingEngine {
public:
  virtual vec3 calculateLight(Ray ray, ivec2 pixel = ivec2(0, 0)) = 0;

  const shared_ptr<const vector<Triangle>> triangles;
  const shared_ptr<const PointLight> light;
  int countedSamples = 1;
  LightingEngine(Scene scene)
      : triangles(scene.triangles), light(scene.light){};
};
