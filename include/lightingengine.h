#pragma once

#include <limits>
#include <omp.h>

#include "camera.h"
#include "cube.h"
#include "light.h"
#include "pointlight.h"
#include "scene.h"
#include "sdlscreen.h"
#include "spherelight.h"
#include "testmodel.h"

using glm::ivec2;
using std::numeric_limits;

class LightingEngine {
public:
  virtual vec3 calculateLight(Ray ray, ivec2 pixel = ivec2(0, 0)) = 0;

  shared_ptr<const vector<Triangle>> triangles;
  shared_ptr<const Light> light;
  int countedSamples = 1;
  LightingEngine(const shared_ptr<const vector<Triangle>> triangles,
                 shared_ptr<Light> light)
      : triangles(triangles), light(light){};
};
