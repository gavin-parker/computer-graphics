#include "pointlight.h"

PointLight::PointLight(vec3 position, vec3 color, float power)
    : position(position), color(color), power(power) {}

void PointLight::update(float dt) {
  Uint8 *keystate = SDL_GetKeyState(0);

  position += velocity * dt *
              vec3(static_cast<float>(keystate[SDLK_j] - keystate[SDLK_l]),
                   static_cast<float>(keystate[SDLK_o] - keystate[SDLK_u]),
                   static_cast<float>(keystate[SDLK_i] - keystate[SDLK_k]));
}

void PointLight::calculateRay(Ray &ray, glm::vec3 target) {
  ray.position = position;
  ray.direction = target - position;
}

// Uses equation 27 on
// https://www.cs.bris.ac.uk/Teaching/Resources/COMS30115/Assignment/2017-COMS30115-1.pdf
// To calculate power of light at an intersection
vec3 PointLight::directLight(const Ray &ray) {
  vec3 offset = position - ray.collisionLocation;

  vec3 light_direction = glm::normalize(offset);
  float radius = glm::length(offset);

  return (std::max(glm::dot(light_direction, ray.collision->normal), 0.0f) *
          power / (4.0f * (static_cast<float>(M_PI)) * radius * radius)) *
         color;
}

vec3 PointLight::vertexLight(Vertex v) {
  vec3 offset = position - v.position;
  vec3 light_direction = glm::normalize(offset);
  float radius = glm::length(offset);

  return (std::max(glm::dot(light_direction, v.normal), 0.0f) * power /
          (4.0f * (static_cast<float>(M_PI)) * radius * radius)) *
         color * v.illumination;
}
