#include "spherelight.h"

SphereLight::SphereLight(vec3 position, vec3 colour, float power, float radius,
                         int res)
    : Light(position, colour, power, res), radius(radius) {}

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

vector<Ray> SphereLight::calculateRays(vec3 target) const {
  vector<Ray> rays;
  rays.reserve(rayCount);
  for (int i = 0; i < rayCount; i++) {
    // pick a random point on the sphere maybe?
    float theta = RAND() * 2 * M_PI;
    float phi = acos(2 * RAND() - 1);
    vec3 point(radius * cos(theta) * sin(phi), radius * sin(theta) * sin(phi),
               radius * cos(phi));
    point += position;
    rays.emplace_back(point, target - point);
  }
  return rays;
}
