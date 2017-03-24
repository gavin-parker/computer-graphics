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
  Ray() : Ray(vec3(), vec3()){};

  Ray(vec3 initPosition, vec3 initDirection,
      float initLength = numeric_limits<float>::max())
      : position(initPosition), direction(initDirection), length(initLength),
        coordinate(Coordinate::None), collision(nullptr) {}

  Ray(const Ray &other)
      : position(other.position), direction(other.direction),
        length(other.length), coordinate(other.coordinate) {
    switch (coordinate) {
    case Coordinate::UV:
      uv = other.uv;
      break;
    case Coordinate::BARY:
      bary = other.bary;
      break;
    default:
      break;
    }
  }

  vec3 getPosition() const { return position; }

  vec3 getDirection() const { return direction; }

  float getLength() const { return length; }

  void extendToInfinity() { length = numeric_limits<float>::max(); }

  Triangle const *getCollision() const { return collision; }

  void updateCollision(Triangle const *newCollision, float newLength) {
    collision = newCollision;
    length = newLength;
    coordinate = Coordinate::None;
  }

  void updateCollision(Triangle const *newCollision, float newLength,
                       vec2 newUV) {
    collision = newCollision;
    length = newLength;
    coordinate = Coordinate::UV;
    uv = newUV;
  }

  void updateCollision(Triangle const *newCollision, float newLength,
                       vec3 newBary) {
    collision = newCollision;
    length = newLength;
    coordinate = Coordinate::BARY;
    bary = newBary;
  }

  vec3 collisionLocation() const {
    switch (coordinate) {
    case Coordinate::UV:
      return collision->getPosition(uv);
    case Coordinate::BARY:
      return collision->getPosition(bary);
    default:
      return position + length * direction;
    }
  }

  vec3 collisionNormal() const {
    switch (coordinate) {
    case Coordinate::UV:
      return collision->getNormal(uv);
    case Coordinate::BARY:
      return collision->getNormal(bary);
    default:
      return collision->normal;
    }
  }

  vec3 collisionAmbientColour() const {
    switch (coordinate) {
    case Coordinate::UV:
      return collision->mat->ambient(collision->getTexUV(uv));
    case Coordinate::BARY:
      return collision->mat->ambient(collision->getTexUV(bary));
    default:
      return collision->mat->ambient();
    }
  }

  vec3 collisionDiffuseColour() const {
    switch (coordinate) {
    case Coordinate::UV:
      return collision->mat->diffuse(collision->getTexUV(uv));
    case Coordinate::BARY:
      return collision->mat->diffuse(collision->getTexUV(bary));
    default:
      return collision->mat->diffuse();
    }
  }

  vec3 collisionSpecularColour(vec3 lightDirection, vec3 lightColour) const {
    switch (coordinate) {
    case Coordinate::UV:
      return collision->specularColour(uv, lightDirection, lightColour,
                                       direction);
    case Coordinate::BARY:
      return collision->specularColour(uv, lightDirection, lightColour,
                                       direction);
    default:
      return collision->specularColour(lightDirection, lightColour, direction);
    }
  }
};
