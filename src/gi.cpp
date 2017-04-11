#include "gi.h"

// GlobalIllumination::GlobalIllumination(){};

GlobalIllumination::GlobalIllumination(const Scene &scene, int sampleCount) : LightingEngine(scene.triangles, scene.light), sampleCount(sampleCount), boundingVolume(scene.volume) {};


vec3 GlobalIllumination::getLightHere(Ray &ray, vec3 reflection) {
	vec3 lightHere(0, 0, 0);
	vector<Ray> rays = light.calculateRays(ray.collisionLocation());
	for (int i = 0; i < light.rayCount; i++) {
		Ray directLightRay = rays[i];

		if (!boundingVolume.calculateAnyIntersection(directLightRay, ray, true)) {
			lightHere += vec3(0, 0, 0);
		}
		else if (directLightRay.getCollision() == ray.getCollision()) {
			lightHere += light.directLight(ray)*ray.getCollision()->mat->getReflection(rays[0].getDirection(), reflection, ray.collisionNormal());
		}
	}
	lightHere /= rays.size();
	return lightHere;
}

vec3 GlobalIllumination::reflectedRatio(vec3 lightIn, vec3 reflection, vec3 normal, const Ptr_Material mat) const {
	return mat->getReflection(lightIn, reflection, normal);
}

vec3 GlobalIllumination::trace(Ray &ray, int bounces) {
	// find diffuse light at this position 
	float diffuse = 0.75f;



	vec3 indirectLight(0, 0, 0);

	// create orthogonal basis on plane 
	vec3 normal = ray.collisionNormal();
	vec3 normalX;
	vec3 normalY;

	if (std::abs(normal.x) > std::abs(normal.y)) {
		normalX = vec3(normal.z, 0, -normal.x) / sqrtf(normal.x*normal.x + normal.z*normal.z);
	}
	else {
		normalX = vec3(0, -normal.z, normal.y) / sqrtf(normal.z*normal.z + normal.y*normal.y);
	}
	normalY = glm::cross(normalX, normal);


	mat3 basis(normalX, normalY, normal);
	if (bounces >= 1) {
			// generate random direction 
			float r1 = RAND();
			float r2 = RAND();
			float sinTheta = sqrtf(1 - r1*r1);
			float phi = 2 * M_PI * r2;
			float x = sinTheta * cosf(phi);
			float z = sinTheta * sinf(phi);
			vec3 sample(x, r1, z);

			vec3 direction(sample.x * normalX.x + sample.y * normal.x + sample.z * normalY.x,
				sample.x * normalX.y + sample.y * normal.y + sample.z * normalY.y,
				sample.x * normalX.z + sample.y * normal.z + sample.z * normalY.z);
			vec3 lightHere = getLightHere(ray, direction);
			vec3 lightRatioReflected = reflectedRatio(direction, ray.getDirection(), ray.collisionNormal(), ray.getCollision()->mat);
			Ray bounce(ray.collisionLocation() + direction*0.001f, direction);
			// return this + new collision point 
			if (boundingVolume.calculateIntersection(bounce)) {
				indirectLight += lightRatioReflected * trace(bounce, bounces - 1);
			}
			else {
				indirectLight += lightRatioReflected * vec3(1, 1, 1)*environment; // assume white environment sphere 
			}
			indirectLight /= static_cast<float>(sampleCount);
			return (lightHere / static_cast<float>(M_PI) + 2.f * indirectLight);
	}
	else {
		return (getLightHere(ray, ray.getDirection()) / static_cast<float>(M_PI));
	}

}

vec3 GlobalIllumination::calculateLight(Ray &ray, ivec2 pixel) {
	return trace(ray, total_bounces);
}
