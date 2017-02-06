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

  const vec3 ambientLight = vec3(0.15f, 0.15f, 0.15f);
  PointLight light;

protected:
  void update(float dt) override;
  void draw(int width, int height) override;

public:
  RayTracer(int width, int height, bool fullscreen = false);
};
