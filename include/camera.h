#pragma once

#define _USE_MATH_DEFINES

#include <SDL.h>
#include <cmath>
#include <iostream>

#include "cube.h"
#include "lerp.h"
#include "ray.h"
#include "vertex.h"

class Camera {
public:
  vec3 position;
  const float speed;
  float yaw;
  const float yawSpeed;
  mat3 rotation;
  const float viewOffset;
  bool moved = false;

  Camera(vec3 position, float speed, float yaw, float yawPeriod,
         float viewAngle);
  Camera(const Cube &bounds, float timePeriod, float yawPeriod,
         float viewAngle);

  static vec3 calculatePosition(const Cube &bounds, float viewAngle);

  static float calculateSpeed(const Cube &bounds, float timePeriod);

  static float calculateYawSpeed(float yawPeriod);

  bool update(float dt);

  Ray calculateRay(float x, float y);

  vec3 projectVertex(Vertex v);

  vec4 clipSpace(Vertex v);

  vec3 worldSpace(vec4 cameraCoordinate);
};
