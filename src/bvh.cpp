#include "bvh.h"

BoundingVolume::BoundingVolume(const shared_ptr<const vector<Triangle>> triangles) {
	for (int i = 0; i < 7; i++) {
		d[i][0] = numeric_limits<float>::max();
		d[i][1] = -numeric_limits<float>::max();
		for (const Triangle &triangle : *triangles) {
			vec3 points[3] = { triangle.v0, triangle.v1, triangle.v2 };
			for (int j = 0; j < 3; j++) {
				float D = normals[i].x*points[j].x + normals[i].y*points[j].y + normals[i].z*points[j].z;
				d[i][0] = std::min(d[i][0], D);
				d[i][1] = std::min(d[1][0], D);
			}
		}
	}

}
bool BoundingVolume::calculateIntersection(const Ray &ray) const{
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

	return true;
}