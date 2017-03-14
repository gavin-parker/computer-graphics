#pragma once

#include "texture.h"

using glm::vec3;

class Material {
private:
  Texture ambientTexture, diffuseTexture, specularTexture,
      specularExponentTexture;

public:
  Material() {}

  Material(vec3 ka, vec3 kd, vec3 ks, vec4::value_type ns, const string &mapKa,
           const string &mapKd, const string &mapKs, const string &mapNs)
      : ambientTexture(vec4(ka, 1.0f), mapKa),
        diffuseTexture(vec4(kd, 1.0f), mapKd),
        specularTexture(vec4(ka, 1.0f), mapKa),
        specularExponentTexture(vec4(1.0f, 1.0f, 1.0f, ns), mapNs) {}

  inline vec3 ambient(vec2 uv) const { return vec3(ambientTexture[uv]); }

  inline vec3 diffuse(vec2 uv) const { return vec3(diffuseTexture[uv]); }

  inline vec3 specular(vec2 uv) const { return vec3(specularTexture[uv]); }

  inline float specularExponent(vec2 uv) const {
    return specularExponentTexture[uv].a;
  }
};
