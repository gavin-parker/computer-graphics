#include "camera.h"

Camera::Camera(vec3 position, float yaw, float viewAngle)
    : position(position), yaw(yaw),
      viewOffset(static_cast<float>(tan(viewAngle * M_PI / 180.f))) {}

void Camera::update(float dt) {
  Uint8 *keystate = SDL_GetKeyState(0);

  yaw += (keystate[SDLK_RIGHT] - keystate[SDLK_LEFT]) * yawVeclocity * dt;

  rotation = mat3(cos(yaw), 0.0f, sin(yaw), 0.0f, 1.0f, 0.0f, -sin(yaw), 0.0f,
                  cos(yaw));

  vec3 right = vec3(1.0f, 0.0f, 0.0f), upwards = vec3(0.0f, 1.0f, 0.0f),
       forwards = vec3(0.0f, 0.0f, -1.0f);

  position +=
      velocity * dt * rotation *
      (static_cast<float>(keystate[SDLK_d] - keystate[SDLK_a]) * right +
       static_cast<float>(keystate[SDLK_e] - keystate[SDLK_q]) * upwards +
       static_cast<float>(keystate[SDLK_w] - keystate[SDLK_s]) * forwards);
}

void Camera::calculateRay(Ray &ray, float x, float y) {
  ray.position = position;

  vec3 cameraSpaceDirection = vec3(lerp(-viewOffset, viewOffset, x),
                                   lerp(viewOffset, -viewOffset, y), -1.0f);

  ray.direction = rotation * cameraSpaceDirection;
}

void Camera::VertexShader(const vec3 &v, ivec2 &p) {
  vec3 newPos = rotation * (v - position);
  float f = 250;
  p = ivec2(f * (newPos.x / newPos.z) + 250, f * (newPos.y / newPos.z) + 250);
}
