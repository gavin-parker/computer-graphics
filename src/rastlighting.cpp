#include "rastlighting.h"

// GlobalIllumination::GlobalIllumination(){};

RastLighting::RastLighting(const shared_ptr<Scene> scene) : LightingEngine(scene->triangles, scene->light), boundingVolume(scene->volume) {};

vec3 RastLighting::calculateLight(Ray ray, ivec2 pixel) {
	vector<Ray> rays;
	vec3 lightColour(0, 0, 0);
	light->calculateRays(rays, ray.collisionLocation);
	for (int i = 0; i < rays.size(); i++) {
		Ray lightRay = rays[i];
		shared_ptr<const Material> mat = ray.collision->mat;
		vec3 n = ray.collision->normal;
		vec3 v = glm::normalize(ray.direction);
		vec3 l = glm::normalize(lightRay.direction);
		vec3 spec = mat->phong(v, l, n);
		lightColour +=
			light->directLight(ray) * mat->diffuse + spec * mat->specularity;
	}
	lightColour /= rays.size();
	return lightColour;
}

bool RastLighting::ClosestIntersection(Ray &ray) {
	ray.length = numeric_limits<float>::max();

	bool anyIntersection = false;

	for (const Triangle &triangle : *triangles) {
		anyIntersection |= triangle.calculateIntersection(ray);
	}

	return anyIntersection;
}

// like closestIntersection, but backs out after a single intersection
bool RastLighting::anyIntersection(Ray &ray, Ray &surface) {
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
