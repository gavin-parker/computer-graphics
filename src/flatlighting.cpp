#include "flatlighting.h"

// GlobalIllumination::GlobalIllumination(){};

FlatLighting::FlatLighting(const shared_ptr<Scene> scene) : LightingEngine(scene->triangles, scene->light), boundingVolume(scene->volume) {
};

vec3 FlatLighting::calculateLight(Ray ray, ivec2 pixel) {

	return ray.collision->colour;
}

bool FlatLighting::ClosestIntersection(Ray &ray) {
	ray.length = numeric_limits<float>::max();

	bool anyIntersection = false;

	for (const Triangle &triangle : *triangles) {
		anyIntersection |= triangle.calculateIntersection(ray);
	}

	return anyIntersection;
}

// like closestIntersection, but backs out after a single intersection
bool FlatLighting::anyIntersection(Ray &ray, Ray &surface) {
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