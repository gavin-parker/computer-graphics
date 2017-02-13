#pragma once
#include <limits>
#include <glm/glm.hpp>
#include "ray.h"
#define R3 static_cast<float>(sqrt(3) / 3.f)
using glm::vec3;
using std::numeric_limits;

//bounding volume is a tighter bounding box, which can contain some geometry as well as other bounding boxes
class BoundingVolume {
private:
	const vec3 normals [7] = { vec3(1,0,0), vec3(0,1,0), vec3(0,0,1), vec3(1,1,1)*R3, vec3(-1,1,1)*R3, vec3(-1,-1,1)*R3, vec3(1,-1,1)*R3 };
	float d[7][2];
	const vector<BoundingVolume> subVolumes;
	const shared_ptr<const vector<Triangle>> triangles;
	bool ClosestIntersection(Ray &ray);

public:
	BoundingVolume(const shared_ptr<const vector<Triangle>> triangles);
	bool calculateIntersection(Ray &ray);
};