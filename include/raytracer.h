#pragma once
#include <limits>
#include <omp.h>
#include "camera.h"
#include "convergent_gi.h"
#include "standardlighting.h"
#include "cube.h"
#include "lightingengine.h"
#include "light.h"
#include "sdlscreen.h"
#include "testmodel.h"
#include "bvh.h"
#include "raycaster.h"

using std::numeric_limits;

class RayTracer : public SdlScreen {
private:
  //bool ClosestIntersection(Ray &ray);
  const shared_ptr<const vector<Triangle>> triangles;
  Camera camera;

  const vec3 ambientLight = vec3(0.1f, 0.1f, 0.1f);
  const shared_ptr<Light> light;
  shared_ptr<LightingEngine> lighting;
  const shared_ptr<BoundingVolume> boundingVolume;
  const shared_ptr<RayCaster> rayCaster;
  bool antialias;
  int chunkSize = 4000;
  vector<int> rayIndices;
protected:
  void update(float dt) override;
  void draw(int width, int height) override;

public:
  RayTracer(int width, int height, shared_ptr<LightingEngine> lighting,
	  const shared_ptr<Light> light,
            const shared_ptr<const vector<Triangle>> triangles, const shared_ptr<BoundingVolume> boundingVolume, const shared_ptr<RayCaster> rayCaster,
            bool fullscreen = false, bool antialias = true);
};
