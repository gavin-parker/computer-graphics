#pragma once

#include "texture.h"

using glm::vec3;

class Material {
private:
  Texture ambientTexture, diffuseTexture, specularTexture,
      specularExponentTexture;

  bool isMirrored;

public:
  Material();

  Material(vec3 ka, vec3 kd, vec3 ks, vec4::value_type ns, bool isMirrored);

  Material(vec3 ka, vec3 kd, vec3 ks, vec4::value_type ns, const string &mapKa,
           const string &mapKd, const string &mapKs, const string &mapNs,
           bool isMirrored);

  vec3 ambient() const;

  vec3 ambient(vec2 uv) const;

  vec3 diffuse() const;

  vec3 diffuse(vec2 uv) const;

  vec3 specular() const;

  vec3 specular(vec2 uv) const;

  float specularExponent() const;

  float specularExponent(vec2 uv) const;
};
