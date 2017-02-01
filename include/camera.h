#pragma once

#include <iostream>
#include <glm/glm.hpp>
#include <SDL.h>

#include "lerp.h"

using glm::vec3;

struct Ray {
	vec3 position;
	vec3 direction;
};

class Camera {
public:
	vec3 position;

	const float viewOffset;
	const float velocity = 1.0f;

	Camera(vec3 position, float viewAngle);

	void Update(float dt);

	void calculateRay(Ray &ray, float x, float y);
};
