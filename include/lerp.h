#pragma once

#include "pixel.h"

// Linearly interpolate between a and b
inline float lerpF(float a, float b, float t) { return a + (b - a) * t; }

inline float deLerpF(float a, float b, float t) { return (t - a) / (b - a); }

inline float deLerp(vec3 a, vec3 b, float t) {
  return glm::length(
      vec3(deLerpF(a.x, b.x, t), deLerpF(a.y, b.y, t), deLerpF(a.z, b.z, t)));
}

inline vec3 lerp(vec3 a, vec3 b, float t) { return a + (b - a) * t; }

inline int lerpI(int a, int b, float t) {
  return static_cast<int>(
      lerpF(static_cast<float>(a), static_cast<float>(b), t));
}

inline Vertex lerpV(Vertex a, Vertex b,float recip_z, float t) {
  return Vertex(lerp(a.position, b.position, t) / recip_z, a.normal, a.reflectance,
                lerp(a.illumination, b.illumination, t) / recip_z);
}

inline Pixel lerpP(Pixel a, Pixel b, float t) {
	float recip_z = lerpF(a.depth, b.depth, t);
  return Pixel(lerpI(a.x, b.x, t), lerpI(a.y, b.y, t),
               recip_z, lerpV(a.v, b.v, recip_z, t));
}
