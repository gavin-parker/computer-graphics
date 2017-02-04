#pragma once

#include <algorithm>
#include <glm/glm.hpp>
#include <vector>

using glm::vec3;
using std::vector;

class Triangle;

#include "ray.h"

class Triangle
{
public:
	const vec3 v0;
	const vec3 e1;
	const vec3 e2;
	const vec3 normal;
	const vec3 color;
	const vector<unsigned char> texture;

	Triangle(vec3 v0, vec3 v1, vec3 v2, vec3 color, vector<unsigned char> &texture);

	bool calculateIntection(Ray &ray) const;

	vec3 getColor(float u, float v) const;

};
