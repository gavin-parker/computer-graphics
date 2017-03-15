#include "convergent_gi.h"

ConvergentGlobalIllumination::ConvergentGlobalIllumination(shared_ptr<Scene> scene,shared_ptr<RayCaster> rayCaster, int sampleCount, int width, int height) : LightingEngine(scene->triangles, scene->light, rayCaster), gi(new GlobalIllumination(scene, rayCaster, sampleCount)), image(vector<vec3>(width*height)), width(width), height(height) {};

vec3 ConvergentGlobalIllumination::calculateLight(Ray ray, ivec2 pixel) {
	vec3 color = gi->calculateLight(ray);
	image[width*pixel.y + pixel.x] += color;
	return image[width*pixel.y + pixel.x] / static_cast<float>(countedSamples);
}