#pragma once

#include <omp.h>

#include "bvh.h"
#include "camera.h"
#include "convergent_gi.h"
#include "cube.h"
#include "light.h"
#include "lightingengine.h"
#include "objectscreen.h"

using std::numeric_limits;

class RayTracer : public ObjectScreen {
private:
  const Ptr_Triangles &triangles;
  const BoundingVolume &boundingVolume;

  bool antialias;
  int chunkSize = 4000;

protected:
  void draw(int width, int height) override;

public:
  RayTracer(int width, int height, float viewAngle, LightingEngine &lighting,
            Scene &scene, bool fullscreen = false, bool antialias = true);
};
