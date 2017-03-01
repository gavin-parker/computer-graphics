#pragma once
#define _USE_MATH_DEFINES
#include <cmath>
#include <SDL.h>
#include <algorithm>
#include <math.h>
#include "lerp.h"
#include "ray.h"
#include "vertex.h"


using glm::ivec2;
using glm::vec3;
using std::numeric_limits;
using std::vector;
class Light {

protected:

public:
	vec3 color;
	float power;

	const float velocity = 200.0f;

	vec3 position;

	virtual void update(float dt) = 0;

	virtual void calculateRays(vector<Ray> &rays, vec3 target) const = 0;

	virtual vec3 directLight(const Ray &ray) const = 0;

	virtual vec3 vertexLight(Vertex v) const = 0;

	Light(vec3 position, vec3 color, float power) : position(position), color(color), power(power) {};

};
