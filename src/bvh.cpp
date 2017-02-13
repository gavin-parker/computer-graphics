#include "bvh.h"

BoundingVolume::BoundingVolume(const shared_ptr<const vector<Triangle>> triangles) : triangles(triangles) {
	for (int i = 0; i < 7; i++) {
		d[i][0] = numeric_limits<float>::max();
		d[i][1] = -numeric_limits<float>::max();
		for (const Triangle &triangle : *triangles) {
			vec3 points[3] = { triangle.v0, triangle.v1, triangle.v2 };
			for (int j = 0; j < 3; j++) {
				float D = normals[i].x*points[j].x + normals[i].y*points[j].y + normals[i].z*points[j].z;
				d[i][0] = std::min(d[i][0], D);
				d[i][1] = std::max(d[i][1], D);
			}
		}
	}

}
//ray must have max length before initial call
bool BoundingVolume::calculateIntersection(Ray &ray) {
	float num[7];
	float denom[7];
	float tFar = numeric_limits<float>::max();
	float tNear = -numeric_limits<float>::max();
	int planeIndex = 0;
	for (int i = 0; i < 7; i++) {
		num[i] = glm::dot(normals[i], ray.position);
		denom[i] = glm::dot(normals[i], ray.direction);
		float tn = (d[i][0] - num[i]) / denom[i];
		float tf = (d[i][1] - num[i]) / denom[i];
		if (denom[i] < 0) std::swap(tn, tf);
		if (tn > tNear) {
			tNear = tn;
			planeIndex = i;
		}
		if (tf < tFar) {
			tFar = tf;
		}
		if (tNear > tFar) {
			return false;
		}
	}

	//first check the geometry with this volume as direct parent
	bool anyIntersection = ClosestIntersection(ray);

	//then check sub volumes if there are any
	for (BoundingVolume volume : subVolumes) {
		anyIntersection |= volume.calculateIntersection(ray);
	}


	return anyIntersection;
}

bool BoundingVolume::ClosestIntersection(Ray &ray) {

	bool anyIntersection = false;

	for (const Triangle &triangle : *triangles) {
		anyIntersection |= triangle.calculateIntersection(ray);
	}

	return anyIntersection;
}
