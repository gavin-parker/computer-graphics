#pragma once

#include <limits>
#include <omp.h>

#include "camera.h"
#include "pointlight.h"
#include "sdlscreen.h"
#include "testmodel.h"

using std::numeric_limits;

class LightingEngine {
public:
  virtual vec3 calculateLight(Ray ray) = 0;

  const shared_ptr<const vector<Triangle>> triangles;
  shared_ptr<PointLight> light;

  LightingEngine(const shared_ptr<const vector<Triangle>> triangles,
                 shared_ptr<PointLight> light)
      : triangles(triangles), light(light){};
};
