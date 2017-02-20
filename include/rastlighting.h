#include "lightingengine.h"
class RastLighting : public LightingEngine {
private:
	bool ClosestIntersection(Ray &ray);
	bool anyIntersection(Ray &ray, Ray &surface);
	int sampleCount = 5;
	vec3 ambientLight = vec3(0.1f, 0.1f, 0.1f);
	const shared_ptr<BoundingVolume> boundingVolume;

protected:
public:
	RastLighting(const shared_ptr<Scene> scene);
	RastLighting();
	virtual vec3 calculateLight(Ray ray, ivec2 pixel = ivec2(0, 0)) override;
};
#pragma once
