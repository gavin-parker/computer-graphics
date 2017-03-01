#pragma once
#include "light.h"
#ifndef unix
#define RAND float(rand()) / RAND_MAX
#else
#define RAND drand48()
#endif

class SphereLight : public Light {
private:

	const int lightRes = 4;
	const float radius = 1.0f;
public:

	SphereLight(vec3 position, vec3 color, float power);

	void update(float dt) override;

	void calculateRays(vector<Ray> &rays, vec3 target) const override;

	vec3 directLight(const Ray &ray) const override;

	vec3 vertexLight(Vertex v) const override;
};
