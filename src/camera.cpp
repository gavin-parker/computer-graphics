#include "camera.h"

Camera::Camera(vec3 position, float speed, float yaw, float yawPeriod,
               float viewAngle)
    : HasSpeed(speed), position(position), speed(speed), yaw(yaw),
      yawSpeed(calculateYawSpeed(yawPeriod)),
      viewOffset(static_cast<float>(tan(viewAngle * M_PI / 180.f))) {}

Camera::Camera(const Cube &bounds, float timePeriod, float yawPeriod,
               float viewAngle)
    : Camera(calculatePosition(bounds, viewAngle),
             calculateSpeed(bounds, timePeriod), 0.0f, yawPeriod, viewAngle) {}

vec3 Camera::calculatePosition(const Cube &bounds, float viewAngle) {
  vec3 faceCenter = vec3(0.5f, 0.5f, 0.0f) * (bounds.a + bounds.b);

  float t = static_cast<float>(tan(viewAngle * M_PI / 180.f));

  float width = bounds.b.x - bounds.a.x;

  return faceCenter - vec3(0.0f, 0.0f, width / (2.0f * t));
}

float Camera::calculateYawSpeed(float yawPeriod) {
  return 2.0f * M_PI / yawPeriod;
}

bool Camera::update(float dt) {
  Uint8 *keystate = SDL_GetKeyState(0);

  yaw += (keystate[SDLK_RIGHT] - keystate[SDLK_LEFT]) * yawSpeed * dt;

  rotation = mat3(cos(yaw), 0.0f, sin(yaw), 0.0f, 1.0f, 0.0f, -sin(yaw), 0.0f,
                  cos(yaw));

  const vec3 right = vec3(1.0f, 0.0f, 0.0f);
  const vec3 upwards = vec3(0.0f, 1.0f, 0.0f);
  const vec3 forwards = vec3(0.0f, 0.0f, 1.0f);

  position +=
      speed * dt * rotation *
      (static_cast<float>(keystate[SDLK_a] - keystate[SDLK_d]) * right +
       static_cast<float>(keystate[SDLK_e] - keystate[SDLK_q]) * upwards +
       static_cast<float>(keystate[SDLK_w] - keystate[SDLK_s]) * forwards);

  return ((keystate[SDLK_a] - keystate[SDLK_d]) != 0 ||
          (keystate[SDLK_e] - keystate[SDLK_q]) != 0 ||
          (keystate[SDLK_w] - keystate[SDLK_s]) != 0);
}

Ray Camera::calculateRay(float x, float y) {

  vec3 cameraSpaceDirection = vec3(lerpF(viewOffset, -viewOffset, x),
                                   lerpF(viewOffset, -viewOffset, y), 1.0f);

  return Ray(position, rotation * cameraSpaceDirection);
}

vec3 Camera::projectVertex(vec3 v) {
  vec3 newPos = (v - position) * rotation;

  return vec3(newPos.x / (viewOffset * newPos.z),
              newPos.y / (viewOffset * newPos.z), newPos.z);
}

vec4 Camera::clipSpace(vec3 v) {
  vec3 newPos = (v - position) * rotation;

  return vec4(newPos, newPos.z / 250.0);
}

vec3 Camera::worldSpace(vec4 cameraCoordinate) {
  vec3 plain(cameraCoordinate.x, cameraCoordinate.y, cameraCoordinate.z);
  vec3 newPos = (rotation * plain) + position;
  return newPos;
}
