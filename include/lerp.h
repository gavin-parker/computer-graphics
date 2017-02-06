#pragma once
#include <glm/glm.hpp>
using glm::vec2;
// Linearly interpolate between a and b
inline float lerp(float a, float b, float t) { return a + (b - a) * t; }

inline float deLerp(float a, float b, float t) { return (t - a) / (b - a); }

inline vec2 lerp(vec2 a, vec2 b, float t) { return a + (b - a) * t; }
