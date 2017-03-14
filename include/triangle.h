#pragma once

#include "material.h"
#include <algorithm>
#include <memory>

using glm::dot;
using glm::normalize;
using glm::reflect;

class Triangle;

#include "ray.h"

class Triangle {
public:
  const vec3 v0, v1, v2, e1, e2;
  const vec2 vt0, vt1, vt2, et1, et2;
  const vec3 vn0, vn1, vn2, en1, en2, normal;
  const Material &mat;

  Triangle(vec3 v0, vec3 v1, vec3 v2, vec2 vt0, vec2 vt1, vec2 vt2, vec3 vn0,
           vec3 vn1, vec3 vn2, const Material &mat);

  bool calculateIntersection(Ray &ray) const;

  inline vec3 getPosition(vec2 uv) const { return v0 + uv.x * e1 + uv.y * e2; }

  inline vec2 getTexUV(vec2 uv) const { return vt0 + uv.x * et1 + uv.y * et2; }

  inline vec3 getNormal(vec2 uv) const {
    return normalize(vn0 + uv.x * en1 + uv.y * en2);
  }

  inline vec3 ambientColour(vec2 uv, vec3 lightColour) const {
    return scaleVec(lightColour, mat.ambient(getTexUV(uv)));
  }

  inline vec3 diffuseColour(vec2 uv, vec3 lightIncidentDirection,
                            vec3 lightColour) const {
    return scaleVec(lightColour, dot(lightIncidentDirection, getNormal(uv)) *
                                     mat.diffuse(getTexUV(uv)));
  }

  inline vec3 specularColour(vec2 uv, vec3 lightIncidentDirection,
                             vec3 lightColour,
                             vec3 cameraIncidentDirection) const {
    auto reflection = normalize(reflect(lightIncidentDirection, getNormal(uv)));
    auto specularCoefficient =
        glm::pow(dot(cameraIncidentDirection, reflection),
                 mat.specularExponent(getTexUV(uv)));

    return scaleVec(lightColour,
                    specularCoefficient * mat.specular(getTexUV(uv)));
  }
};
