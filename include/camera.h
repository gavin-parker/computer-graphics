#ifndef CAMERA_H
#define CAMERA_H

#include <iostream>
#include <glm/glm.hpp>
#include <SDL.h>

class Camera
{
public:
	glm::vec3 position;
	float focalLength;

	const float velocity = 1.0f;

	Camera(glm::vec3 position, float focalLength);

	void Update(float dt);
};

#endif
