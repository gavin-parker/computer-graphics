#pragma once
#define _USE_MATH_DEFINES
#include <SDL.h>
#include <cmath>
#include <glm/glm.hpp>
#include <iostream>
#include <limits>

#include "lerp.h"
#include "ray.h"
#include "vertex.h"

using glm::vec3;
using glm::vec2;
using glm::vec4;

using std::numeric_limits;

class Camera {
private:
  const float velocity = 200.0f;
  const float yawVeclocity = 2.0f;

public:
  vec3 position;
  float yaw;
  mat3 rotation;
  const float viewOffset;
  bool moved = false;
  Camera(vec3 position, float yaw, float viewAngle);

  bool update(float dt);

  Ray calculateRay(float x, float y);

  vec3 projectVertex(Vertex v);

  vec4 clipSpace(Vertex v);
};
