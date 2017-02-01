#pragma once

#include <glm/glm.hpp>

using glm::vec3;

class Triangle
{
public:
	const vec3 v0;
	const vec3 v1;
	const vec3 v2;
	const vec3 normal;
	const vec3 color;

	Triangle(vec3 v0, vec3 v1, vec3 v2, vec3 color);

	static vec3 getNormal(vec3 v0, vec3 v1, vec3 v2);
};
