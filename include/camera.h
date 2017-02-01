#pragma once

#include <iostream>
#include <glm/glm.hpp>
#include <SDL.h>

using glm::vec3;

class Camera {
public:
	vec3 position;
	float focalLength;

	const float velocity = 1.0f;

	Camera(vec3 position, float focalLength);

	void Update(float dt);
};
