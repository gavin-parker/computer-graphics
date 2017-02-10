#pragma once
#define _USE_MATH_DEFINES
#include <cmath>
#include <SDL.h>
#include <glm/glm.hpp>
#include <iostream>

#include "lerp.h"
#include "ray.h"
#include "vertex.h"

using glm::vec3;
using glm::vec2;

class Camera {
private:
  float yaw;
  mat3 rotation;
  const float viewOffset;
  const float velocity = 200.0f;
  const float yawVeclocity = 2.0f;

public:
	vec3 position;

  Camera(vec3 position, float yaw, float viewAngle);

  void update(float dt);

  void calculateRay(Ray &ray, float x, float y);

  vec3 projectVertex(Vertex v);
};
