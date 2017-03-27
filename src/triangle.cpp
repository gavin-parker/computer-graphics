#include "triangle.h"

vec3 Triangle::calculateNormal(vec3 v0, vec3 v1, vec3 v2) {
  return glm::normalize(glm::cross(v2 - v0, v1 - v0));
}

Triangle::Triangle(vec3 v0, vec3 v1, vec3 v2, vec2 vt0, vec2 vt1, vec2 vt2,
                   shared_ptr<const Material> mat)
    : v0(v0), v1(v1), v2(v2), e1(v1 - v0), e2(v2 - v0), vt0(vt0), vt1(vt1),
      vt2(vt2), et1(vt1 - vt0), et2(vt2 - vt0),
      vn0(calculateNormal(v0, v1, v2)), vn1(calculateNormal(v0, v1, v2)),
      vn2(calculateNormal(v0, v1, v2)), en1(vn1 - vn0), en2(vn2 - vn0),
      normal(calculateNormal(v0, v1, v2)), mat(mat), reflective(mat->isMirrored), refractive(false) {}

Triangle::Triangle(vec3 v0, vec3 v1, vec3 v2, vec2 vt0, vec2 vt1, vec2 vt2,
                   vec3 vn0, vec3 vn1, vec3 vn2, shared_ptr<const Material> mat)
    : v0(v0), v1(v1), v2(v2), e1(v1 - v0), e2(v2 - v0), vt0(vt0), vt1(vt1),
      vt2(vt2), et1(vt1 - vt0), et2(vt2 - vt0), vn0(vn0), vn1(vn1), vn2(vn2),
      en1(vn1 - vn0), en2(vn2 - vn0), normal(calculateNormal(v0, v1, v2)),
      mat(mat), reflective(mat->isMirrored), refractive(false) {}

bool Triangle::calculateIntersection(Ray &ray) const {
  if (glm::dot(normal, ray.getDirection()) < 0) {
    vec3 b = ray.getPosition() - v0;

    glm::mat3 A(-ray.getDirection(), e1, e2);
    auto det_A = glm::determinant(A);

    auto t = glm::determinant(glm::mat3(b, e1, e2)) / det_A;
    if (t >= 0 && t < ray.getLength()) {
      auto u = glm::determinant(glm::mat3(-ray.getDirection(), b, e2)) / det_A;
      auto v = glm::determinant(glm::mat3(-ray.getDirection(), e1, b)) / det_A;

      if (u >= 0.0f && v >= 0.0f && (u + v) < 1.0f) {
        ray.updateCollision(this, t, vec2(u, v));
        return true;
      }
    }
  }

  return false;
}

// Position

vec3 Triangle::getPosition(vec2 uv) const { return v0 + uv.x * e1 + uv.y * e2; }

vec3 Triangle::getPosition(vec3 bary) const {
  return bary.x * v0 + bary.y * v1 + bary.z * v2;
}

// Texture coordinates

vec2 Triangle::getTexUV(vec2 uv) const { return vt0 + uv.x * et1 + uv.y * et2; }

vec2 Triangle::getTexUV(vec3 bary) const {
  return bary.x * vt0 + bary.y * vt1 + bary.z * vt2;
}

// Normals

vec3 Triangle::getNormal(vec2 uv) const {
  return normalize(vn0 + uv.x * en1 + uv.y * en2);
}

vec3 Triangle::getNormal(vec3 bary) const {
  return normalize(bary.x * vn0 + bary.y * vn1 + bary.z * vn2);
}

// Ambient Colour

vec3 Triangle::ambientColourNorm(vec2 uv, vec3 lightColour) const {
  return scaleVec(lightColour, mat->ambient(uv));
}

vec3 Triangle::ambientColour(vec2 uv, vec3 lightColour) const {
  return ambientColourNorm(getTexUV(uv), lightColour);
}

vec3 Triangle::ambientColour(vec3 bary, vec3 lightColour) const {
  return ambientColourNorm(getTexUV(bary), lightColour);
}

// Diffuse Colour

vec3 Triangle::diffuseColourNorm(vec2 uv, vec3 lightIncidentDirection,
                                 vec3 surfaceNormal, vec3 lightColour) const {
  return scaleVec(lightColour, dot(lightIncidentDirection, surfaceNormal) *
                                   mat->diffuse(uv));
}

vec3 Triangle::diffuseColour(vec2 uv, vec3 lightIncidentDirection,
                             vec3 lightColour) const {
  return diffuseColourNorm(getTexUV(uv), lightIncidentDirection, getNormal(uv),
                           lightColour);
}

vec3 Triangle::diffuseColour(vec3 bary, vec3 lightIncidentDirection,
                             vec3 lightColour) const {
  return diffuseColourNorm(getTexUV(bary), lightIncidentDirection,
                           getNormal(bary), lightColour);
}

// Specular Colour

vec3 Triangle::specularColourNorm(vec3 specular, float specularExponent,
                                  vec3 lightIncidentDirection,
                                  vec3 surfaceNormal, vec3 lightColour,
                                  vec3 cameraIncidentDirection) const {
  auto reflection = normalize(reflect(lightIncidentDirection, surfaceNormal));
  auto specularCoefficient =
      glm::pow(dot(cameraIncidentDirection, reflection), specularExponent);

  return scaleVec(lightColour, specularCoefficient * specular);
}

vec3 Triangle::specularColour(vec3 lightIncidentDirection, vec3 lightColour,
                              vec3 cameraIncidentDirection) const {
  return specularColourNorm(mat->specular(), mat->specularExponent(),
                            lightIncidentDirection, normal, lightColour,
                            cameraIncidentDirection);
}

vec3 Triangle::specularColour(vec2 uv, vec3 lightIncidentDirection,
                              vec3 lightColour,
                              vec3 cameraIncidentDirection) const {
  return specularColourNorm(mat->specular(getTexUV(uv)),
                            mat->specularExponent(getTexUV(uv)),
                            lightIncidentDirection, getNormal(uv), lightColour,
                            cameraIncidentDirection);
}

vec3 Triangle::specularColour(vec3 bary, vec3 lightIncidentDirection,
                              vec3 lightColour,
                              vec3 cameraIncidentDirection) const {
  return specularColourNorm(mat->specular(getTexUV(bary)),
                            mat->specularExponent(getTexUV(bary)),
                            lightIncidentDirection, getNormal(bary),
                            lightColour, cameraIncidentDirection);
}
