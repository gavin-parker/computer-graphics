#include "material.h"

Material::Material() {}

bool Material::loadPNG(string filename) {
  std::cout << "loading texture: " << filename;
  unsigned error = lodepng::decode(texture, width, height, filename);

  if (error) {
    std::cout << "Error loading texture: " << error << ":"
              << lodepng_error_text(error);
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
