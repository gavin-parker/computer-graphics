#pragma once

#include "texture.h"

using glm::vec3;

class Material {
private:
  Texture ambientTexture, diffuseTexture, specularTexture,
      specularExponentTexture;

public:
  Material();

  Material(vec3 ka, vec3 kd, vec3 ks, vec4::value_type ns, const string &mapKa,
           const string &mapKd, const string &mapKs, const string &mapNs);

  vec3 ambient(vec2 uv) const;

  vec3 diffuse(vec2 uv) const;

  vec3 specular(vec2 uv) const;
};
