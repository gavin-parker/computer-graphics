#pragma once
#include "ray.h"
#include "bvh.h"
class RayCaster {


public:

	bool castRay(Ray ray);

	RayCaster(const shared_ptr<const vector<Triangle>> triangles, const shared_ptr<BoundingVolume> boundingVolume);


};