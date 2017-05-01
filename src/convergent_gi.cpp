#include "convergent_gi.h"

ConvergentGlobalIllumination::ConvergentGlobalIllumination(const Scene &scene,
	int sampleCount,
	int width,
	int height)
	: LightingEngine(scene.triangles, scene.light), gi(scene, sampleCount),
	image(vector<vec3>(width * height)), width(width), height(height) {};

vec3 ConvergentGlobalIllumination::calculateLight(Ray &ray, ivec2 pixel) {
	vec3 color = gi.calculateLight(ray);;
	color = vec3(std::min(color.x, 1.f), std::min(color.y, 1.f), std::min(color.z, 1.f));
	image[width * pixel.y + pixel.x] += color;
	return image[width * pixel.y + pixel.x] / static_cast<float>(countedSamples);
}
