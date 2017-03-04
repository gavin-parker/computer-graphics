#pragma once
#define _USE_MATH_DEFINES
#include <cmath>
#include <SDL.h>
#include <algorithm>
#include <math.h>
#include "lerp.h"
#include "ray.h"
#include "vertex.h"

#define RIGHT vec3(1,0,0)
#define LEFT vec3(-1,0,0)
#define UP vec3(0,1,0)
#define DOWN vec3(0,-1,0)
#define FORWARD vec3(0,0,1)
#define BACK vec3(0,0,-1)
#define EMPTY vec3(0,0,0)
#define ROTATIONS {mat3(RIGHT,EMPTY,EMPTY), mat3(LEFT,EMPTY,EMPTY), mat3(EMPTY,UP,EMPTY), mat3(EMPTY,DOWN,EMPTY), mat3(EMPTY,EMPTY,FORWARD), mat3(EMPTY,EMPTY,BACK)}

using glm::ivec2;
using glm::vec3;
using std::numeric_limits;
using std::vector;
class Light {
protected:
	int width;
	int height;

public:
	vec3 color;
	float power;

	const float velocity = 200.0f;

	int rayCount = 1;

	vec3 position;

	virtual void update(float dt) = 0;

	virtual void calculateRays(vector<Ray> &rays, vec3 target) const = 0;

	virtual vec3 directLight(const Ray &ray) const = 0;

	virtual vec3 vertexLight(Vertex v) const = 0;

	Light(vec3 position, vec3 color, float power, int rayCount, int width = 500, int height = 500) : position(position), color(color), power(power), rayCount(rayCount), width(width), height(height) {};

	int projectVertex(vec3 vert, float& depth) {

		const mat3 rotations[6] = ROTATIONS;
		for (int i = 0; i < 6; i++) {
			mat3 rotation = rotations[i];
			vec3 newPos = (vert - position) * rotation;
			int x = width * (1 - newPos.x) / 2.0;
			int y = height * (1 - newPos.y) / 2.0;
			if (x >= 0 && x < width && y >= 0 && y <= height && abs(newPos.z) < numeric_limits<float>::max()) {
				depth = abs(newPos.z);
				return i;
			}
			else {
				depth = std::numeric_limits<float>::max();
			}
		}
		return 0;
	}
};
