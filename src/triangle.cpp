#include "triangle.h"

Triangle::Triangle(vec3 v0, vec3 v1, vec3 v2, vec2 vt0, vec2 vt1, vec2 vt2,
                   vec3 vn0, vec3 vn1, vec3 vn2, const Material &mat)
    : v0(v0), v1(v1), v2(v2), e1(v1 - v0), e2(v2 - v0), vt0(vt0), vt1(vt1),
      vt2(vt2), et1(vt1 - vt0), et2(vt2 - vt0), vn0(vn0), vn1(vn1), vn2(vn2),
      en1(vn1 - vn0), en2(vn2 - vn0),
      normal(glm::normalize(glm::cross(v2 - v0, v1 - v0))), mat(mat) {}

bool Triangle::calculateIntersection(Ray &ray) const {
  if (glm::dot(normal, ray.direction) < 0) {
    vec3 b = ray.position - v0;

    glm::mat3 A(-ray.direction, e1, e2);
    auto det_A = glm::determinant(A);

    auto t = glm::determinant(glm::mat3(b, e1, e2)) / det_A;
    if (t >= 0 && t < ray.length) {
      auto u = glm::determinant(glm::mat3(-ray.direction, b, e2)) / det_A;
      auto v = glm::determinant(glm::mat3(-ray.direction, e1, b)) / det_A;

      if (u >= 0.0f && v >= 0.0f && (u + v) < 1.0f) {
        ray.collision = this;
        ray.length = t;
        ray.uv = vec2(u, v);
        return true;
      }
    }
  }

  return false;
}
