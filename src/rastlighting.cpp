#include "rastlighting.h"

// GlobalIllumination::GlobalIllumination(){};

RastLighting::RastLighting(const shared_ptr<Scene> scene, int lightMapResolution) : LightingEngine(scene->triangles, scene->light), boundingVolume(scene->volume), lightMapResolution(lightMapResolution), lightMap(vector<const Triangle*>(lightMapResolution*lightMapResolution)) {
	buidLightMap(lightMapResolution);
};

vec3 RastLighting::calculateLight(Ray ray, ivec2 pixel) {
	vec3 lightColour(0, 0, 0);
	Ray lightRay;
	lightRay.direction = ray.collisionLocation - light->position;
	shared_ptr<const Material> mat = ray.collision->mat;
	vec3 n = ray.collision->normal;
	vec3 v = glm::normalize(ray.direction);
	vec3 l = glm::normalize(lightRay.direction);
	vec3 spec = mat->phong(v, l, n);
	if (mappedIntersection(l) == ray.collision) {
		lightColour +=
			light->directLight(ray) * mat->diffuse + spec * mat->specularity;
	}
	else {
		lightColour +=
			ambientLight* mat->diffuse;
	}

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

const Triangle* RastLighting::mappedIntersection(vec3 direction) {
	//theta is 0 - 2pi, phi is 0 - pi
	//get vector to point on light sphere
	float theta = atan2(direction.y , direction.x) + M_PI;
	theta = theta/(M_PI*2);
	float phi = acos(direction.z);
	phi = phi / M_PI;
	int x = floor(phi*lightMapResolution);
	int y = floor(theta*lightMapResolution);
	return lightMap[y*lightMapResolution + x];
}

//builds a light map for current light
void RastLighting::buidLightMap(int resolution) {
	float increment = 1.f / static_cast<float>(lightMapResolution);
	for (int y = 0; y < lightMapResolution; y++) {
		float theta = static_cast<float>(y)*increment;
		theta = theta * 2 * M_PI - M_PI;
		for (int x = 0; x < lightMapResolution; x++) {
			float phi = static_cast<float>(x)*increment;
			phi *= M_PI;
			vec3 point(cos(theta)*sin(phi), sin(theta)*sin(phi), cos(phi));
			Ray ray;
			ray.position = light->position;
			ray.direction = point;
			ClosestIntersection(ray);
			lightMap[y*lightMapResolution + x] = ray.collision;
		}
	}

}

