#pragma once

#include <algorithm>
#include <glm/glm.hpp>
#include <math.h>
#include <SDL.h>

#include "lerp.h"

using glm::vec3;
using glm::mat3;

class PointLight {
private:
	vec3 position;
	vec3 color;
	float power;

	const float velocity = 200.0f;

public:
	PointLight(vec3 position, vec3 color, float power);

	void update(float dt);

	vec3 directLight(vec3 point, vec3 normal);
};
