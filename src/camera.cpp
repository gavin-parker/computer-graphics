#include "camera.h"

Camera::Camera(vec3 position, float yaw, float viewAngle)
    : position(position), yaw(yaw),
      viewOffset(static_cast<float>(tan(viewAngle * M_PI / 180.f))) {}

Camera::Camera(const Cube &bounds, float viewAngle)
    : Camera(calculatePosition(bounds, viewAngle), 0.0f, viewAngle) {}

vec3 Camera::calculatePosition(const Cube &bounds, float viewAngle) {
  vec3 faceCenter = vec3(0.5f, 0.5f, 0.0f) * (bounds.a + bounds.b);

  float t = static_cast<float>(tan(viewAngle * M_PI / 180.f));

  float width = bounds.b.x - bounds.a.x;

  return faceCenter - vec3(0.0f, 0.0f, width / (2.0f * t));
}

bool Camera::update(float dt) {
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

  return ((keystate[SDLK_a] - keystate[SDLK_d]) != 0 ||
          (keystate[SDLK_e] - keystate[SDLK_q]) != 0 ||
          (keystate[SDLK_w] - keystate[SDLK_s]) != 0);
}

Ray Camera::calculateRay(float x, float y) {

  vec3 cameraSpaceDirection = vec3(lerpF(viewOffset, -viewOffset, x),
                                   lerpF(viewOffset, -viewOffset, y), 1.0f);

  return Ray(position, rotation * cameraSpaceDirection);
}

vec3 Camera::projectVertex(Vertex v) {
  vec3 newPos = (v.position - position) * rotation;

  return vec3((newPos.x / newPos.z), (newPos.y / newPos.z), newPos.z);
}

vec4 Camera::clipSpace(Vertex v) {
  vec3 newPos = (v.position - position) * rotation;

  return vec4(newPos, newPos.z / 250.0);
}

vec3 Camera::worldSpace(vec4 cameraCoordinate) {
  vec3 plain(cameraCoordinate.x, cameraCoordinate.y, cameraCoordinate.z);
  vec3 newPos = (rotation * plain) + position;
  return newPos;
}
