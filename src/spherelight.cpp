#include "spherelight.h"

SphereLight::SphereLight(vec3 position, vec3 color, float power, float radius,
                         int res)
    : Light(position, color, power, res), radius(radius) {}

bool SphereLight::update(float dt) {
  Uint8 *keystate = SDL_GetKeyState(0);

  position += velocity * dt *
              vec3(static_cast<float>(keystate[SDLK_j] - keystate[SDLK_l]),
                   static_cast<float>(keystate[SDLK_o] - keystate[SDLK_u]),
                   static_cast<float>(keystate[SDLK_i] - keystate[SDLK_k]));

  if ((keystate[SDLK_j] - keystate[SDLK_l]) != 0 ||
      (keystate[SDLK_o] - keystate[SDLK_u]) != 0 ||
      (keystate[SDLK_i] - keystate[SDLK_k]) != 0) {
    return true;
  }
  return false;
}

void SphereLight::calculateRays(vector<Ray> &rays, glm::vec3 target) const {
  for (int i = 0; i < rayCount; i++) {
    // pick a random point on the sphere maybe?
    float theta = RAND() * 2 * M_PI;
    float phi = acos(2 * RAND() - 1);
    vec3 point(radius * cos(theta) * sin(phi), radius * sin(theta) * sin(phi),
               radius * cos(phi));
    point += position;
    rays[i] = Ray(point, target - point);
  }
}

// Uses equation 27 on
// https://www.cs.bris.ac.uk/Teaching/Resources/COMS30115/Assignment/2017-COMS30115-1.pdf
// To calculate power of light at an intersection
vec3 SphereLight::directLight(const Ray &ray) const {
  vec3 offset = position - ray.collisionLocation;

  vec3 light_direction = glm::normalize(offset);
  float radius = glm::length(offset);

  return (std::max(glm::dot(light_direction, ray.collision->normal), 0.0f) *
          power / (4.0f * (static_cast<float>(M_PI)) * radius * radius)) *
         color;
}

vec3 SphereLight::vertexLight(Vertex v) const {
  vec3 offset = position - v.position;
  vec3 light_direction = glm::normalize(offset);
  float radius = glm::length(offset);

  return (std::max(glm::dot(light_direction, v.normal), 0.0f) * power /
          (4.0f * (static_cast<float>(M_PI)) * radius * radius)) *
         color * v.illumination;
}
