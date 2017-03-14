#include "triangle.h"

Triangle::Triangle(vec3 v0, vec3 v1, vec3 v2, vec2 vt0, vec2 vt1, vec2 vt2,
                   vec3 colour, const shared_ptr<const Material> mat, bool refractive, bool reflective)
    : v0(v0), v1(v1), v2(v2), e1(v1 - v0), e2(v2 - v0), vt0(vt0), vt1(vt1),
      vt2(vt2), et1(vt1 - vt0), et2(vt2 - vt0),
      normal(glm::normalize(glm::cross(v2 - v0, v1 - v0))), colour(colour),
      mat(mat), refractive(refractive), reflective(reflective) {}

bool Triangle::calculateIntersection(Ray &ray) const {
  if (glm::dot(normal, ray.direction) < 0) {

    vec3 b = ray.position - v0;

    glm::mat3 A(-ray.direction, e1, e2);
    float det_A = glm::determinant(A);

    float t = glm::determinant(glm::mat3(b, e1, e2)) / det_A;
    if (t >= 0 && t < ray.length) {
      float u = glm::determinant(glm::mat3(-ray.direction, b, e2)) / det_A;
      float v = glm::determinant(glm::mat3(-ray.direction, e1, b)) / det_A;

      if (u >= 0 && v >= 0 && (u + v) < 1) {
        ray.length = t;
        ray.collision = this;
        ray.collisionLocation = v0 + u * e1 + v * e2;
        ray.collisionUVLocation = vt0 + u * et1 + v * et2;
        return true;
      }
    }
  }

  return false;
}

vec3 Triangle::getColour(vec2 uv) const {
  vec3 matColour = mat->getColour(uv);
  return vec3(matColour.r * colour.r, matColour.g * colour.g,
              matColour.b * colour.b);
}

vec3 Triangle::getColour(vec3 bary) const {
	vec2 uv = vt0*bary.x + vt1*bary.y + vt2*bary.z;
	vec3 matColour = mat->getColour(uv);
	return vec3(matColour.r * colour.r, matColour.g * colour.g,
		matColour.b * colour.b);
}

vec3 Triangle::getPixelColour(vec2 uv) const {
#ifndef textureLess
	return mat->getColour(uv); 
#else
	return colour;
#endif
}
