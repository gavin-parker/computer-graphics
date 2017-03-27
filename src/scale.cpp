#include "scale.h"

vec2 scaleVec(const vec2 &a, const vec2 &b) {
  return vec2(a.x * b.x, a.y * b.y);
}

vec3 scaleVec(const vec3 &a, const vec3 &b) {
  return vec3(a.x * b.x, a.y * b.y, a.z * b.z);
}

vec4 scaleVec(const vec4 &a, const vec4 &b) {
  return vec4(a.x * b.x, a.y * b.y, a.z * b.z, a.w * b.w);
}
