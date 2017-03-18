#pragma once

#include <glm/glm.hpp>

using glm::vec2;
using glm::vec3;
using glm::mat3;

struct Ray;

#include "triangle.h"

class Ray {
public:
  vec3 position;
  vec3 direction;

  Triangle const *collision;
  vec3 collisionLocation;
  vec2 collisionUVLocation;
  vec3 collisionNormal;
  float length;

  void updateCollision(Triangle const *newCollision, vec2 uv) {
    collision = newCollision;
    collisionLocation = collision->getPosition(uv);
    collisionUVLocation = collision->getTexUV(uv);
    collisionNormal = collision->getNormal(uv);
  }

  void updateCollision(Triangle const *newCollision, float u, float v) {
    updateCollision(newCollision, vec2(u, v));
  }
};
