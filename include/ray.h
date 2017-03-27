#pragma once

#include <glm/glm.hpp>
#include <limits>
#include <utility>

using glm::vec2;
using glm::vec3;
using glm::mat3;

using std::numeric_limits;

class Ray;

#include "triangle.h"

class Ray {
private:
  enum class Coordinate { None, UV, BARY };

  vec3 position;
  vec3 direction;

  float length;

  Coordinate coordinate;

  union {
    vec2 uv;
    vec3 bary;
  };

  Triangle const *collision;

public:
  Ray();

  Ray(vec3 initPosition, vec3 initDirection,
      float initLength = numeric_limits<float>::max());

  Ray(const Ray &other);

  vec3 getPosition() const;

  vec3 getDirection() const;

  float getLength() const;

  void extendToInfinity();

  Triangle const *getCollision() const;

  void updateCollision(Triangle const *newCollision, float newLength);

  void updateCollision(Triangle const *newCollision, float newLength,
                       vec2 newUV);

  void updateCollision(Triangle const *newCollision, float newLength,
                       vec3 newBary);

  vec3 collisionLocation() const;

  vec3 collisionNormal() const;

  vec3 collisionAmbientColour() const;

  vec3 collisionDiffuseColour() const;

  vec3 collisionSpecularColour(vec3 lightDirection, vec3 lightColour) const;
};
