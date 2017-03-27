#include "material.h"

Material::Material() {}

Material::Material(vec3 ka, vec3 kd, vec3 ks, vec4::value_type ns,
                   bool isMirrored)
    : Material(ka, kd, ks, ns, "", "", "", "", isMirrored) {}

Material::Material(vec3 ka, vec3 kd, vec3 ks, vec4::value_type ns,
                   const string &mapKa, const string &mapKd,
                   const string &mapKs, const string &mapNs, bool isMirrored)
    : ambientTexture(vec4(ka, 1.0f), mapKa),
      diffuseTexture(vec4(kd, 1.0f), mapKd),
      specularTexture(vec4(ka, 1.0f), mapKa),
      specularExponentTexture(vec4(1.0f, 1.0f, 1.0f, ns), mapNs),
      isMirrored(isMirrored) {}

vec3 Material::ambient() const { return vec3(ambientTexture.getScale()); }

vec3 Material::ambient(vec2 uv) const { return vec3(ambientTexture[uv]); }

vec3 Material::diffuse() const { return vec3(diffuseTexture.getScale()); }

vec3 Material::diffuse(vec2 uv) const { return vec3(diffuseTexture[uv]); }

vec3 Material::specular() const { return vec3(specularTexture.getScale()); }

vec3 Material::specular(vec2 uv) const { return vec3(specularTexture[uv]); }

float Material::specularExponent() const {
  return specularExponentTexture.getScale().a;
}

float Material::specularExponent(vec2 uv) const {
  return specularExponentTexture[uv].a;
}
