#include "light.h"

Light::Light(vec3 position, vec3 colour, float power, int rayCount, int width,
             int height)
    : position(position), colour(colour), power(power), rayCount(rayCount),
      width(width), height(height){};

vec3 Light::directLight(const Ray &ray) const {
  return colour * power / (4.0f * (static_cast<float>(M_PI)) * ray.getLength() *
                           ray.getLength());
}

indexedPixel Light::projectVertex(vec3 vert, float &depth) {
  depth = numeric_limits<float>::max();
  for (int i = 0; i < 6; i++) {
    vec3 newPos = (vert - position) * rotations[i];
    float xf = newPos.x / newPos.z;
    float yf = newPos.y / newPos.z;
    int x = static_cast<int>(round(width * (1 - xf) / 2.0));
    int y = static_cast<int>(round(height * (1 - yf) / 2.0));
    if (x >= 0 && x < width && y >= 0 && y < height && newPos.z > 0.f) {
      depth = newPos.z;
      return {x, y, i};
    }
  }
  return {-1, -1, -1};
}
