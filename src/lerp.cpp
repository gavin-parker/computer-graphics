#include "lerp.h"

float lerpF(float a, float b, float t) { return a + (b - a) * t; }

float deLerpF(float a, float b, float t) { return (t - a) / (b - a); }

int lerpI(int a, int b, float t) {
  return static_cast<int>(
      lerpF(static_cast<float>(a), static_cast<float>(b), t));
}

float deLerpI(int a, int b, int t) {
  return deLerpF(static_cast<float>(a), static_cast<float>(b),
                 static_cast<float>(t));
}

vec3 lerpV(vec3 a, vec3 b, float t) {
  return vec3(lerpF(a.x, b.x, t), lerpF(a.y, b.y, t), lerpF(a.z, b.z, t));
}

vec3 lerpV(vec3 a, vec3 b, vec3 t) {
  return vec3(lerpF(a.x, b.x, t.x), lerpF(a.y, b.y, t.y), lerpF(a.z, b.z, t.z));
}

Pixel lerpP(Pixel a, Pixel b, float t) {
  return Pixel(lerpI(a.x, b.x, t), lerpI(a.y, b.y, t),
               lerpF(a.depth, b.depth, t), lerpV(a.position, b.position, t));
}
