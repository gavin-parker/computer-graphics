#include "light.h"

Light::Light(vec3 position, vec3 colour, float power, int rayCount, int width,
             int height)
    : position(position), colour(colour), power(power), rayCount(rayCount),
      width(width), height(height){};

vec3 Light::directLight(const Ray &ray) const {
  return colour * power / (4.0f * (static_cast<float>(M_PI)) * ray.getLength() *
                           ray.getLength());
}

vec3 Light::vertexLight(Vertex v) const {
  vec3 offset = position - v.position;
  vec3 light_direction = glm::normalize(offset);
  float radius = glm::length(offset);

  return (std::max(glm::dot(light_direction, v.normal), 0.0f) * power /
          (4.0f * (static_cast<float>(M_PI)) * radius * radius)) *
         colour * v.illumination;
}

indexedPixel Light::projectVertex(vec3 vert, float &depth) {
  depth = numeric_limits<float>::max();
  for (int i = 0; i < 6; i++) {
    vec3 newPos = (vert - position) * rotations[i];
    float xf = static_cast<float>((newPos.x / newPos.z));
    float yf = static_cast<float>((newPos.y / newPos.z));
    int x = static_cast<int>(width * (1 - xf) / 2.0);
    int y = static_cast<int>(height * (1 - yf) / 2.0);
    if (x >= 0 && x < width && y >= 0 && y < height &&
        newPos.z < numeric_limits<float>::max() && newPos.z > 0.f) {
      depth = newPos.z;
      return {x, y, i};
    }
  }
  return {-1, -1, -1};
}
