#pragma once
#include "lightingengine.h"
#include "bvh.h"
class Scene {
private:
public:
  const shared_ptr<PointLight> light;
  const shared_ptr<const vector<Triangle>> triangles;
  const shared_ptr<BoundingVolume> volume;

  Scene(shared_ptr<PointLight> light,
        shared_ptr<const vector<Triangle>> triangles, shared_ptr<BoundingVolume> volume)
      : light(light), triangles(triangles), volume(volume){};
};
