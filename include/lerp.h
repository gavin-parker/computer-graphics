#pragma once

#include <glm/glm.hpp>

using glm::vec2;
using glm::ivec2;

// Linearly interpolate between a and b
inline float lerp(float a, float b, float t) { return a + (b - a) * t; }

inline float deLerp(float a, float b, float t) { return (t - a) / (b - a); }

inline vec2 lerp(vec2 a, vec2 b, float t) { return a + (b - a) * t; }

inline ivec2 lerp(ivec2 a, ivec2 b, float t) {
  return ivec2(lerp(a.x, b.x, t), lerp(a.y, b.y, t));
}
