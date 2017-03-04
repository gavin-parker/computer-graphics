#include "lightingengine.h"
class RastLighting : public LightingEngine {
private:
	bool ClosestIntersection(Ray &ray);
	bool anyIntersection(Ray &ray, Ray &surface);
	int sampleCount = 5;
	vec3 ambientLight = vec3(0.1f, 0.1f, 0.1f);
	const shared_ptr<BoundingVolume> boundingVolume;
	int lightMapResolution;
	vector<const Triangle*> lightMap;
	void buidLightMap(int resolution);
	const Triangle* mappedIntersection(vec3 direction);
protected:
public:
	RastLighting(const shared_ptr<Scene> scene, int lightMapResolution = 1000);
	RastLighting();
	virtual vec3 calculateLight(Ray ray, ivec2 pixel = ivec2(0, 0)) override;
};
#pragma once
