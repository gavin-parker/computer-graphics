#pragma once
#include "lightingengine.h"
#include "bvh.h"
class Scene {
private:
public:
  const shared_ptr<Light> light;
  const shared_ptr<const vector<Triangle>> triangles;
  const shared_ptr<BoundingVolume> volume;

  Scene(shared_ptr<Light> light,
        shared_ptr<const vector<Triangle>> triangles, shared_ptr<BoundingVolume> volume)
      : light(light), triangles(triangles), volume(volume){};
};
