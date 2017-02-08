#pragma once

#include <limits>
#include <omp.h>

#include "camera.h"
#include "pointlight.h"
#include "sdlscreen.h"
#include "testmodel.h"

using std::numeric_limits;

class RayTracer : public SdlScreen {
private:
  bool ClosestIntersection(Ray &ray);
  const shared_ptr<const vector<Triangle>> triangles;

  Camera camera;

  const vec3 ambientLight = vec3(0.1f, 0.1f, 0.1f);
  PointLight light;

  vec3 globalIllumination(Ray ray, int bounces);
  int sampleCount = 100;

protected:
  void update(float dt) override;
  void draw(int width, int height) override;

public:
  RayTracer(int width, int height, bool fullscreen = false);
};
