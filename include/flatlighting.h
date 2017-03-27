#pragma once
#include "lightingengine.h"
class FlatLighting : public LightingEngine {
private:
	bool ClosestIntersection(Ray &ray);
	bool anyIntersection(Ray &ray, Ray &surface);
	int sampleCount = 5;
	vec3 ambientLight = vec3(0.1f, 0.1f, 0.1f);
	const BoundingVolume &boundingVolume;
protected:
public:
	FlatLighting(Scene &scene);
	FlatLighting();
	virtual vec3 calculateLight(Ray ray, ivec2 pixel = ivec2(0, 0));
};
#pragma once
