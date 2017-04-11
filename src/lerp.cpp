#include "lerp.h"

float lerpF(float a, float b, float t) { return a + (b - a) * t; }

float deLerpF(float a, float b, float t) { return (t - a) / (b - a); }

float deLerp(vec3 a, vec3 b, float t) {
  return glm::length(
      vec3(deLerpF(a.x, b.x, t), deLerpF(a.y, b.y, t), deLerpF(a.z, b.z, t)));
}

vec3 lerp(vec3 a, vec3 b, float t) { return a + (b - a) * t; }

vec3 lerp(vec3 a, vec3 b, vec3 t) { return a + (b - a) * t; }

int lerpI(int a, int b, float t) {
  return static_cast<int>(
      lerpF(static_cast<float>(a), static_cast<float>(b), t));
}

Vertex lerpV(Vertex a, Vertex b, float a_z, float b_z, float c_z, float t) {
  return Vertex(lerp(a.position, b.position, t), a.normal, a.reflectance,
                lerp(a.illumination / a_z, b.illumination / b_z, t) * c_z);
}

Pixel lerpP(Pixel a, Pixel b, float t) {
  float c_z = lerpF(a.depth, b.depth, t);
  return Pixel(lerpI(a.x, b.x, t), lerpI(a.y, b.y, t), c_z,
               lerpV(a.v, b.v, a.depth, b.depth, c_z, t));
}
