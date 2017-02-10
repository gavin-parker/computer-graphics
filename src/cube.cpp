#include "cube.h"


Cube::Cube(vec3 a, vec3 b) : a(a), b(b) {};

Cube::Cube(const shared_ptr<const vector<Triangle>> triangles){
	float maxFloat = numeric_limits<float>::max();
	vec3 maxBound(-maxFloat, -maxFloat, -maxFloat);
	vec3 minBound(maxFloat, maxFloat, maxFloat);

	for (const Triangle &triangle : *triangles) {

		vec3 verts[3] = { triangle.v0, triangle.v0 + triangle.e1, triangle.v0 + triangle.e2 };

		for (int i = 0; i < 3; i++) {
			maxBound.x = std::max(verts[i].x, maxBound.x);
			maxBound.y = std::max(verts[i].y, maxBound.y);
			maxBound.z = std::max(verts[i].z, maxBound.z);
			minBound.x = std::min(verts[i].x, minBound.x);
			minBound.y = std::min(verts[i].y, minBound.y);
			minBound.z = std::min(verts[i].z, minBound.z);

		}
	}
	a = minBound;
	b =  maxBound;


};


bool Cube::calculateIntersection(Ray &ray) const {
	float tmin = (a.x - ray.position.x) / ray.direction.x;
	float tmax = (b.x - ray.position.x) / ray.direction.x;

	if (tmin > tmax) {
		float t = tmin;
		tmin = tmax;
		tmax = t;
	}

	float tymin = (a.y - ray.position.y) / ray.direction.y;
	float tymax = (b.y - ray.position.y) / ray.direction.y;

	if (tymin > tymax) {
		float t = tymin;
		tymin = tymax;
		tymax = t;
	}
	if ((tmin > tymax) || (tymin > tmax)) {
		return false;
	}
	if (tymin > tmin) {
		tmin = tymin;
	}
	if (tymax < tmax) {
		tmax = tymax;
	}

	float tzmin = (a.z - ray.position.z) / ray.direction.z;
	float tzmax = (b.z - ray.position.z) / ray.direction.z;

	if (tzmin > tzmax) {
		float t = tzmin;
		tzmin = tzmax;
		tzmax = t;
	}
	if ((tmin > tzmax) || (tzmin > tmax)) {
		return false;
	}
	if (tzmin > tmin) {
		tmin = tzmin;
	}
	if (tzmax < tmax) {
		tmax = tzmax;
	}

	return true;
};
