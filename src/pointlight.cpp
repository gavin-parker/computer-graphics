#include "pointlight.h"

PointLight::PointLight(vec3 position, vec3 colour, float power)
    : Light(position, colour, power, 1) {}

bool PointLight::update(float dt) {
  Uint8 *keystate = SDL_GetKeyState(0);

  position += speed * dt *
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

vector<Ray> PointLight::calculateRays(vec3 target) const {
  vector<Ray> rays;
  rays.emplace_back(position, target - position);
  return rays;
}
