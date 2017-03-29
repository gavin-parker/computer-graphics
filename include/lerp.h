#pragma once

#include "pixel.h"
typedef struct Line {
	vec4 a;
	vec4 b;
} Line;

// Linearly interpolate between a and b
float lerpF(float a, float b, float t);

float deLerpF(float a, float b, float t);

float deLerp(vec3 a, vec3 b, float t);

vec3 lerp(vec3 a, vec3 b, float t);

int lerpI(int a, int b, float t);

Vertex lerpV(Vertex a, Vertex b, float a_z, float b_z, float c_z, float t);

Pixel lerpP(Pixel a, Pixel b, float t);


//given a line and a new 2d point, finds the new corresponding 3d point on the line
inline vec4 pointOnLine(Line line, vec2 newPoint) {
	vec2 a(line.a.x, line.a.y);
	vec2 b(line.b.x, line.b.y);
	float original = glm::distance(a, b);


	float newDist = glm::length(newPoint - a);
	if (original < 1 || newDist > original) {
		return line.a;
	}
	float t = newDist / original;

	return vec4(newPoint.x, newPoint.y, lerpF(line.a.z, line.b.z, t), lerpF(line.a.w, line.b.w, t));
}
