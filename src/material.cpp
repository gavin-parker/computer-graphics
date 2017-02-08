#include "material.h"

Material::Material() {}

bool Material::loadPNG(string filename) {
  cout << "loading texture: " << filename << endl;
  unsigned error = lodepng::decode(texture, width, height, filename);

  if (error) {
    cout << "Error loading texture: " << error << ":"
         << lodepng_error_text(error) << endl;
    return false;
  } else {
    return true;
  }
}

unsigned normaliseCoordinate(float position, unsigned size) {
  signed sSize = static_cast<signed>(size);

  return ((static_cast<signed>(position * static_cast<float>(size)) % sSize) +
          sSize) %
         sSize;
}

vec3 Material::getColour(vec2 uv) const {
  unsigned x = uv.x * width;
  unsigned y = uv.y * height;

  size_t index = (width * y + x) * 4;
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
