#include "gi.h"

// GlobalIllumination::GlobalIllumination(){};

GlobalIllumination::GlobalIllumination(shared_ptr<Scene> scene, shared_ptr<RayCaster> rayCaster, int sampleCount) : LightingEngine(scene->triangles, scene->light, rayCaster), sampleCount(sampleCount), boundingVolume(scene->volume) {};

vec3 GlobalIllumination::trace(Ray ray, int bounces) {
	// find diffuse light at this position
	float diffuse = ray.collision->mat->diffuse;
	vec3 lightHere(0, 0, 0);

	vector<Ray> rays(light->rayCount);
	light->calculateRays(rays, ray.collisionLocation);
	vector<int> lightIndices(light->rayCount);
	for (int i = 0; i < light->rayCount; i++) {
		Ray directLightRay = rays[i];

#pragma omp critical
		{
			lightIndices[i] = rayCaster->enqueueRay(rays[i]);
		}
	}
	vec3 indirectLight(0, 0, 0);

	// create orthogonal basis on plane
	vec3 normal = ray.collision->normal;
	vec3 normalX;
	vec3 normalY;

	if (abs(normal.x) > abs(normal.y)) {
		normalX = vec3(normal.z, 0, -normal.x) / sqrtf(normal.x*normal.x + normal.z*normal.z);
	}
	else {
		normalX = vec3(0, -normal.z, normal.y) / sqrtf(normal.z*normal.z + normal.y*normal.y);
	}
	normalY = glm::cross(normalX, normal);


	mat3 basis(normalX, normalY, normal);
	vector<int> bounceIndices(sampleCount);
	vector<float> angles(sampleCount);
	if (bounces >= 1) {
		for (int i = 0; i < sampleCount; i++) {

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

			Ray bounce;
			bounce.position = ray.collisionLocation;
			bounce.direction = glm::normalize(direction);
			bounce.length = std::numeric_limits<float>::max();
			#pragma omp critical
			{
				bounceIndices[i] = rayCaster->enqueueRay(bounce);
			}
			// return this + new collision point
			angles[i] = r1;
		}
	}
#pragma omp barrier
#pragma omp single
	{
		rayCaster->castRays();
	}
	//accumulate direct light
	for (int i = 0; i < light->rayCount; i++) {
		bool collision = false;
		Ray lightRay = rayCaster->getRay(lightIndices[i], collision);
		if (collision) {
			lightHere += light->directLight(ray)*ray.collision->getPixelColour(ray.collisionUVLocation);
		}
		else {
			lightHere += vec3(0, 0, 0);
		}
	}
	lightHere /= rays.size();
	if (bounces >= 1) {
		//accumulate indirect light
		for (int i = 0; i < sampleCount; i++) {
			bool collision = false;
			Ray bounce = rayCaster->getRay(bounceIndices[i], collision);
			float r1 = angles[i];
			if (collision) {
				indirectLight += r1 * trace(bounce, bounces - 1);
			}
			else {
				indirectLight += r1 * vec3(1, 1, 1)*environment;
			}
		}
	}

	if (bounces < 1) {
		return (lightHere / static_cast<float>(M_PI))*diffuse;
	}
	indirectLight /= static_cast<float>(sampleCount);
	return (lightHere / static_cast<float>(M_PI) + 2.f * indirectLight)*diffuse;
}

vec3 GlobalIllumination::calculateLight(Ray ray, ivec2 pixel) {
	return trace(ray, total_bounces)* ray.collision->getPixelColour(ray.collisionUVLocation);
}


