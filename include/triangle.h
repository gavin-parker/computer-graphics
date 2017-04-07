#pragma once

#include "material.h"
#include <algorithm>

using glm::dot;
using glm::normalize;
using glm::reflect;

class Triangle;

typedef const Triangle *Ptr_Triangle;
typedef vector<Ptr_Triangle> Ptr_Triangles;

#include "ray.h"

class Triangle {
public:
  const vec3 v0, v1, v2, e1, e2;
  const vec2 vt0, vt1, vt2, et1, et2;
  const vec3 vn0, vn1, vn2, en1, en2, normal;
  const Ptr_Material mat;

  static vec3 calculateNormal(vec3 v0, vec3 v1, vec3 v2);

  Triangle();

  Triangle(const Triangle &other) = delete;

  Triangle(vec3 v0, vec3 v1, vec3 v2, vec2 vt0, vec2 vt1, vec2 vt2,
           const Material *const mat);

  Triangle(vec3 v0, vec3 v1, vec3 v2, vec2 vt0, vec2 vt1, vec2 vt2, vec3 vn0,
           vec3 vn1, vec3 vn2, const Material *const mat);

  bool calculateIntersection(Ray &ray) const;

  // Position

  vector<vec3> getVertices() const;

  vec3 getPosition(vec2 uv) const;

  vec3 getPosition(vec3 bary) const;

  // Texture coordinates

  vec2 getTexUV(vec2 uv) const;

  vec2 getTexUV(vec3 bary) const;

  // Normals

  vec3 getNormal(vec2 uv) const;

  vec3 getNormal(vec3 bary) const;

  // Ambient Colour

  vec3 ambientColourNorm(vec2 uv, vec3 lightColour) const;

  vec3 ambientColour(vec2 uv, vec3 lightColour) const;

  vec3 ambientColour(vec3 bary, vec3 lightColour) const;

  // Diffuse Colour

  vec3 diffuseColourNorm(vec2 uv, vec3 lightIncidentDirection,
                         vec3 surfaceNormal, vec3 lightColour) const;

  vec3 diffuseColour(vec2 uv, vec3 lightIncidentDirection,
                     vec3 lightColour) const;

  vec3 diffuseColour(vec3 bary, vec3 lightIncidentDirection,
                     vec3 lightColour) const;

  // Specular Colour

  vec3 specularColourNorm(vec3 specular, float specularExponent,
                          vec3 lightIncidentDirection, vec3 surfaceNormal,
                          vec3 lightColour, vec3 cameraIncidentDirection) const;

  vec3 specularColour(vec3 lightIncidentDirection, vec3 lightColour,
                      vec3 cameraIncidentDirection) const;

  vec3 specularColour(vec2 uv, vec3 lightIncidentDirection, vec3 lightColour,
                      vec3 cameraIncidentDirection) const;

  vec3 specularColour(vec3 bary, vec3 lightIncidentDirection, vec3 lightColour,
                      vec3 cameraIncidentDirection) const;

  bool isMirrored() const;
};
