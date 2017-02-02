#pragma once

#include <iostream>
#include <SDL.h>

#include "lerp.h"
#include "ray.h"

class Camera {
private:
	vec3 position;
	float yaw;
	mat3 rotation;
	const float viewOffset;

	const float velocity = 200.0f;
	const float yawVeclocity = 2.0f;

public:
	Camera(vec3 position, float yaw, float viewAngle);

	void update(float dt);

	void calculateRay(Ray &ray, float x, float y);
};
