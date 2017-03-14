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


bool BoundingVolume::calculateIntersection(Ray &ray, bool topVolume) {
	float num[7];
	float denom[7];
	for (int i = 0; i < 7; i++) {
		num[i] = glm::dot(normals[i], ray.position);
		denom[i] = glm::dot(normals[i], ray.direction);
	}
	bool intersection =  calculateIntersectionSub(ray, num, denom);
	if (intersection && topVolume && ray.collision->reflective) {
		vec3 reflection = ray.direction - 2.f*(ray.collision->normal * ray.direction)*ray.collision->normal;
		Ray reflectedRay;
		reflectedRay.position = ray.collisionLocation;
		reflectedRay.direction = reflection;
		reflectedRay.length = numeric_limits<float>::max();
		bool bounce = calculateIntersection(reflectedRay, true);
		ray.collision = reflectedRay.collision;
		ray.collisionLocation = reflectedRay.collisionLocation;
		ray.collisionUVLocation = reflectedRay.collisionUVLocation;
		return bounce;
	}
	else {
		return intersection;
	}
}

//ray must have max length before initial call
bool BoundingVolume::calculateIntersectionSub(Ray &ray, float num[7], float denom[7]) {
	float tFar = numeric_limits<float>::max();
	float tNear = -numeric_limits<float>::max();
	for (int i = 0; i < 7; i++) {
		float tn = (d[i][0] - num[i]) / denom[i];
		float tf = (d[i][1] - num[i]) / denom[i];
		if (denom[i] < 0) std::swap(tn, tf);
		tNear = (tn > tNear) ? tn : tNear;
		tFar = (tf < tFar) ? tf : tFar;
		if (tNear > tFar) {
			return false;
		}
	}

	//first check the geometry with this volume as direct parent
	bool anyIntersection = ClosestIntersection(ray);

	//then check sub volumes if there are any
	for (size_t i = 0; i < subVolumes.size(); i++) {
		anyIntersection |= subVolumes[i].calculateIntersectionSub(ray, num, denom);
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

void BoundingVolume::setSubVolume(BoundingVolume volume) {
	subVolumes.push_back(volume);
}


// recursively checks for ANY intersection, backs out early
bool BoundingVolume::calculateAnyIntersection(Ray &ray, Ray &surface, bool topVolume) {
	float lightDistance = ray.length;
	float num[7];
	float denom[7];
	float tFar = numeric_limits<float>::max();
	float tNear = -numeric_limits<float>::max();
	for (int i = 0; i < 7; i++) {
		num[i] = glm::dot(normals[i], ray.position);
		denom[i] = glm::dot(normals[i], ray.direction);
		float tn = (d[i][0] - num[i]) / denom[i];
		float tf = (d[i][1] - num[i]) / denom[i];
		if (denom[i] < 0) std::swap(tn, tf);
		tNear = (tn > tNear) ? tn : tNear;
		tFar = (tf < tFar) ? tf : tFar;
		if (tNear > tFar) {
			return false;
		}
	}

	//first check the geometry with this volume as direct parent
	bool anyIntersection = this->anyIntersection(ray, surface);

	//then check sub volumes if there are any
	for (BoundingVolume& volume : subVolumes) {
		anyIntersection |= volume.calculateIntersection(ray);
		if (anyIntersection && ray.collision != surface.collision &&
			ray.length < lightDistance) {
			break;
		}
	}
	if (anyIntersection && topVolume && ray.collision->reflective) {
		vec3 reflection = ray.direction - 2.f*(ray.collision->normal * ray.direction)*ray.collision->normal;
		Ray reflectedRay;
		reflectedRay.position = ray.collisionLocation;
		reflectedRay.direction = reflection;
		reflectedRay.length = numeric_limits<float>::max();
		bool bounce = calculateAnyIntersection(reflectedRay, surface, true);
		ray.collision = reflectedRay.collision;
		ray.collisionLocation = reflectedRay.collisionLocation;
		ray.collisionUVLocation = reflectedRay.collisionUVLocation;
		return bounce;
	}

	return anyIntersection;
}

bool BoundingVolume::anyIntersection(Ray &ray, Ray &surface) {
	bool anyIntersection = false;
	float lightDistance = ray.length;
	ray.length = numeric_limits<float>::max();
	for (const Triangle &triangle : *triangles) {
		anyIntersection |= triangle.calculateIntersection(ray);
		if (anyIntersection && ray.collision != surface.collision &&
			ray.length < lightDistance) {
			return anyIntersection;
		}
	}
	return anyIntersection;
}