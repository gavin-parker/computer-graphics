#pragma once

#include "pixel.h"
typedef struct Line {
	vec4 a;
	vec4 b;
} Line;

// Linearly interpolate between a and b
inline float lerpF(float a, float b, float t) { return a + (b - a) * t; }

inline float deLerpF(float a, float b, float t) { return (t - a) / (b - a); }

inline float deLerp(vec3 a, vec3 b, float t) {
  return glm::length(
      vec3(deLerpF(a.x, b.x, t), deLerpF(a.y, b.y, t), deLerpF(a.z, b.z, t)));
}

inline vec3 lerp(vec3 a, vec3 b, float t) { return a + (b - a) * t; }

inline vec4 lerp(vec4 a, vec4 b, float t) { return a + (b - a) * t; }

inline vec4 persp_lerp(vec4 a, vec4 b, float t) {
	float f = a.z*a.w;
	vec3 worldCoord = lerp(vec3(a.x, a.y, a.z), vec3(b.x, b.y, b.z), t);

	return vec4(worldCoord, worldCoord.z / 250.f);
}


inline int lerpI(int a, int b, float t) {
  return static_cast<int>(
      lerpF(static_cast<float>(a), static_cast<float>(b), t));
}

inline Vertex lerpV(Vertex a, Vertex b,float a_z, float b_z, float c_z, float t) {
  return Vertex(lerp(a.position, b.position, t), a.normal, a.reflectance,
                lerp(a.illumination / a_z, b.illumination / b_z, t) * c_z);
}

inline Pixel lerpP(Pixel a, Pixel b, float t) {
	float c_z = lerpF(a.depth, b.depth, t);
  return Pixel(lerpI(a.x, b.x, t), lerpI(a.y, b.y, t),
               c_z, lerpV(a.v, b.v,a.depth, b.depth, c_z, t));
}

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

	if (t > 1) {
		printf("FUCK\n");
	}
	return vec4(newPoint.x, newPoint.y, lerpF(line.a.z, line.b.z, t), lerpF(line.a.w, line.b.w, t));
}
