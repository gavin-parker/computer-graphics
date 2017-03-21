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
  vec3 v0, v1, v2, e1, e2;
  vec2 vt0, vt1, vt2, et1, et2;
  vec3 vn0, vn1, vn2, en1, en2, normal;
  const shared_ptr<Material> mat;

  inline static vec3 calculateNormal(vec3 v0, vec3 v1, vec3 v2) {
    return glm::normalize(glm::cross(v2 - v0, v1 - v0));
  }

  Triangle();

  Triangle(vec3 v0, vec3 v1, vec3 v2, vec2 vt0, vec2 vt1, vec2 vt2,
           const shared_ptr<Material> mat);

  Triangle(vec3 v0, vec3 v1, vec3 v2, vec2 vt0, vec2 vt1, vec2 vt2, vec3 vn0,
           vec3 vn1, vec3 vn2, const shared_ptr<Material> mat);

  bool calculateIntersection(Ray &ray) const;

  // Position

  inline vec3 getPosition(vec2 uv) const { return v0 + uv.x * e1 + uv.y * e2; }

  inline vec3 getPosition(vec3 bary) const {
    return bary.x * v0 + bary.y * v1 + bary.z * v2;
  }

  // Texture coordinates

  inline vec2 getTexUV(vec2 uv) const { return vt0 + uv.x * et1 + uv.y * et2; }

  inline vec2 getTexUV(vec3 bary) const {
    return bary.x * vt0 + bary.y * vt1 + bary.z * vt2;
  }

  // Normals

  inline vec3 getNormal(vec2 uv) const {
    return normalize(vn0 + uv.x * en1 + uv.y * en2);
  }

  inline vec3 getNormal(vec3 bary) const {
    return normalize(bary.x * vn0 + bary.y * vn1 + bary.z * vn2);
  }

  // Ambient Colour

  inline vec3 ambientColourNorm(vec2 uv, vec3 lightColour) const {
    return scaleVec(lightColour, mat->ambient(uv));
  }

  inline vec3 ambientColour(vec2 uv, vec3 lightColour) const {
    return ambientColourNorm(getTexUV(uv), lightColour);
  }

  inline vec3 ambientColour(vec3 bary, vec3 lightColour) const {
    return ambientColourNorm(getTexUV(bary), lightColour);
  }

  // Diffuse Colour

  inline vec3 diffuseColourNorm(vec2 uv, vec3 lightIncidentDirection,
                                vec3 surfaceNormal, vec3 lightColour) const {
    return scaleVec(lightColour, dot(lightIncidentDirection, surfaceNormal) *
                                     mat->diffuse(uv));
  }

  inline vec3 diffuseColour(vec2 uv, vec3 lightIncidentDirection,
                            vec3 lightColour) const {
    return diffuseColourNorm(getTexUV(uv), lightIncidentDirection,
                             getNormal(uv), lightColour);
  }

  inline vec3 diffuseColour(vec3 bary, vec3 lightIncidentDirection,
                            vec3 lightColour) const {
    return diffuseColourNorm(getTexUV(bary), lightIncidentDirection,
                             getNormal(bary), lightColour);
  }

  // Specular Colour

  inline vec3 specularColourNorm(vec3 specular, float specularExponent,
                                 vec3 lightIncidentDirection,
                                 vec3 surfaceNormal, vec3 lightColour,
                                 vec3 cameraIncidentDirection) const {
    auto reflection = normalize(reflect(lightIncidentDirection, surfaceNormal));
    auto specularCoefficient =
        glm::pow(dot(cameraIncidentDirection, reflection), specularExponent);

    return scaleVec(lightColour, specularCoefficient * specular);
  }

  inline vec3 specularColour(vec3 lightIncidentDirection, vec3 lightColour,
                             vec3 cameraIncidentDirection) const {
    return specularColourNorm(mat->specular(), mat->specularExponent(),
                              lightIncidentDirection, normal, lightColour,
                              cameraIncidentDirection);
  }

  inline vec3 specularColour(vec2 uv, vec3 lightIncidentDirection,
                             vec3 lightColour,
                             vec3 cameraIncidentDirection) const {
    return specularColourNorm(mat->specular(getTexUV(uv)),
                              mat->specularExponent(getTexUV(uv)),
                              lightIncidentDirection, getNormal(uv),
                              lightColour, cameraIncidentDirection);
  }

  inline vec3 specularColour(vec3 bary, vec3 lightIncidentDirection,
                             vec3 lightColour,
                             vec3 cameraIncidentDirection) const {
    return specularColourNorm(mat->specular(getTexUV(bary)),
                              mat->specularExponent(getTexUV(bary)),
                              lightIncidentDirection, getNormal(bary),
                              lightColour, cameraIncidentDirection);
  }
};
