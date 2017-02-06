#include "camera.h"

Camera::Camera(vec3 position, float yaw, float viewAngle)
    : position(position), yaw(yaw),
      viewOffset(static_cast<float>(tan(viewAngle * M_PI / 180.f))) {}

void Camera::update(float dt) {
  Uint8 *keystate = SDL_GetKeyState(0);

  yaw += (keystate[SDLK_RIGHT] - keystate[SDLK_LEFT]) * yawVeclocity * dt;

  rotation = mat3(cos(yaw), 0.0f, sin(yaw), 0.0f, 1.0f, 0.0f, -sin(yaw), 0.0f,
                  cos(yaw));

  const vec3 right = vec3(1.0f, 0.0f, 0.0f);
  const vec3 upwards = vec3(0.0f, 1.0f, 0.0f);
  const vec3 forwards = vec3(0.0f, 0.0f, 1.0f);

  position +=
      velocity * dt * rotation *
      (static_cast<float>(keystate[SDLK_a] - keystate[SDLK_d]) * right +
       static_cast<float>(keystate[SDLK_e] - keystate[SDLK_q]) * upwards +
       static_cast<float>(keystate[SDLK_w] - keystate[SDLK_s]) * forwards);
}

void Camera::calculateRay(Ray &ray, float x, float y) {
  ray.position = position;

  vec3 cameraSpaceDirection = vec3(lerp(viewOffset, -viewOffset, x),
                                   lerp(viewOffset, -viewOffset, y), 1.0f);

  ray.direction = rotation * cameraSpaceDirection;
}

vec2 Camera::VertexShader(vec3 v) {
  vec3 newPos = (v - position) * rotation;
  return vec2((newPos.x / newPos.z), (newPos.y / newPos.z));
}
