#pragma once

#include <algorithm>
#include <math.h>
#include <SDL.h>

#include "lerp.h"
#include "ray.h"

class PointLight {
private:
	vec3 position;
	vec3 color;
	float power;

	const float velocity = 200.0f;

public:
	PointLight(vec3 position, vec3 color, float power);

	void update(float dt);

	vec3 directLight(const Ray &ray);
};
