#include <glm/glm.hpp>

using glm::vec2;
using glm::vec3;
using glm::vec4;

inline vec2 scaleVec(const vec2 &a, const vec2 &b) {
  return vec2(a.x * b.x, a.y * b.y);
}

inline vec3 scaleVec(const vec3 &a, const vec3 &b) {
  return vec3(a.x * b.x, a.y * b.y, a.z * b.z);
}

inline vec4 scaleVec(const vec4 &a, const vec4 &b) {
  return vec4(a.x * b.x, a.y * b.y, a.z * b.z, a.w * b.w);
}
