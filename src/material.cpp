#include "material.h"

Material::Material() {}

Material::Material(vec3 ka, vec3 kd, vec3 ks, vec4::value_type ns,
                   const string &mapKa, const string &mapKd,
                   const string &mapKs, const string &mapNs)
    : ambientTexture(vec4(ka, 1.0f), mapKa),
      diffuseTexture(vec4(kd, 1.0f), mapKd),
      specularTexture(vec4(ka, 1.0f), mapKa),
      specularExponentTexture(vec4(1.0f, 1.0f, 1.0f, ns), mapNs) {}

vec3 Material::ambient(vec2 uv) const { return vec3(ambientTexture[uv]); }

vec3 Material::diffuse(vec2 uv) const { return vec3(diffuseTexture[uv]); }

vec3 Material::specular(vec2 uv) const {
  return specularExponentTexture[uv].a, vec3(specularTexture[uv]);
}

/*vec3 Material::getColour(vec2 uv) const {
  unsigned x = std::floor(uv.x * width);
  unsigned y = std::floor(uv.y * height);

  size_t index =
      min((width * y + x) * 4, static_cast<unsigned>(texture.size() - 4));
  float r = static_cast<float>(texture[index + 0]) / 255.0f;
  float g = static_cast<float>(texture[index + 1]) / 255.0f;
  float b = static_cast<float>(texture[index + 2]) / 255.0f;
  return vec3(r, g, b);
}

vec3 Material::phong(vec3 view, vec3 light, vec3 normal) const {
  vec3 r = light - 2.f * (normal * light) * normal;
  vec3 spec = glm::pow((view * r), vec3(specular_falloff));
  float power = glm::length(spec);
  return vec3(power, power, power);
}
*/
