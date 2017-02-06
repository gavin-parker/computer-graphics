#pragma once

#include "pixel.h"

// Linearly interpolate between a and b
inline float lerpF(float a, float b, float t) { return a + (b - a) * t; }

inline float deLerpF(float a, float b, float t) { return (t - a) / (b - a); }

inline int lerpI(int a, int b, float t) {
  return static_cast<int>(
      lerpF(static_cast<float>(a), static_cast<float>(b), t));
}

inline Pixel lerpP(Pixel a, Pixel b, float t) {
  return Pixel(lerpI(a.x, b.x, t), lerpI(a.y, b.y, t),
               lerpF(a.depth, b.depth, t));
}
