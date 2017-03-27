#pragma once
#include <limits>
#include <omp.h>
#include "camera.h"
#include "convergent_gi.h"
#include "standardlighting.h"
#include "cube.h"
#include "light.h"
#include "lightingengine.h"
#include "sdlscreen.h"
#include "testmodel.h"
#include "bvh.h"

using std::numeric_limits;

class RayTracer : public SdlScreen {
private:
  Camera camera;

  const vec3 ambientLight = vec3(0.1f, 0.1f, 0.1f);

  LightingEngine &lighting;
  Light &light;
  const vector<Triangle> &triangles;
  const BoundingVolume &boundingVolume;

  bool antialias;
  int chunkSize = 4000;
protected:
  void update(float dt) override;
  void draw(int width, int height) override;

public:
  RayTracer(int width, int height, LightingEngine &lighting, Light &light,
            const vector<Triangle> &triangles,
            const BoundingVolume &boundingVolume, bool fullscreen,
            bool antialias = true);
};
