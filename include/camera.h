#pragma once

#include <iostream>
#include <glm/glm.hpp>
#include <SDL.h>

#include "lerp.h"

using glm::vec3;
using glm::mat3;

struct Ray {
	vec3 position;
	vec3 direction;
};

class Camera {
private:
	vec3 position;
	float yaw;
	mat3 rotation;
	const float viewOffset;

	const float velocity = 100.0f;
	const float yawVeclocity = 1.0f;

public:
	Camera(vec3 position, float viewAngle);

	void update(float dt);

	void calculateRay(Ray &ray, float x, float y);
};
