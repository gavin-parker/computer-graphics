#include "ray.h"

Ray::Ray() : Ray(vec3(), vec3()){};

Ray::Ray(vec3 initPosition, vec3 initDirection, float initLength)
    : position(initPosition), direction(initDirection), length(initLength),
      coordinate(Coordinate::None), collision(nullptr) {}

Ray::Ray(const Ray &other)
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

vec3 Ray::getPosition() const { return position; }

vec3 Ray::getDirection() const { return direction; }

float Ray::getLength() const { return length; }

void Ray::extendToInfinity() { length = numeric_limits<float>::max(); }

Triangle const *Ray::getCollision() const { return collision; }

void Ray::updateCollision(Triangle const *newCollision, float newLength) {
  collision = newCollision;
  length = newLength;
  coordinate = Coordinate::None;
}

void Ray::updateCollision(Triangle const *newCollision, float newLength,
                          vec2 newUV) {
  collision = newCollision;
  length = newLength;
  coordinate = Coordinate::UV;
  uv = newUV;
}

void Ray::updateCollision(Triangle const *newCollision, float newLength,
                          vec3 newBary) {
  collision = newCollision;
  length = newLength;
  coordinate = Coordinate::BARY;
  bary = newBary;
}

vec3 Ray::collisionLocation() const {
  switch (coordinate) {
  case Coordinate::UV:
    return collision->getPosition(uv);
  case Coordinate::BARY:
    return collision->getPosition(bary);
  default:
    return position + length * direction;
  }
}

vec3 Ray::collisionNormal() const {
  switch (coordinate) {
  case Coordinate::UV:
    return collision->getNormal(uv);
  case Coordinate::BARY:
    return collision->getNormal(bary);
  default:
    return collision->normal;
  }
}

vec3 Ray::collisionAmbientColour() const {
  switch (coordinate) {
  case Coordinate::UV:
    return collision->mat->ambient(collision->getTexUV(uv));
  case Coordinate::BARY:
    return collision->mat->ambient(collision->getTexUV(bary));
  default:
    return collision->mat->ambient();
  }
}

vec3 collisionAmbientColour(vec3 lightColour) const {
  return scaleVec(lightColour, collisionAmbientColour());
}

vec3 Ray::collisionDiffuseColour() const {
  switch (coordinate) {
  case Coordinate::UV:
    return collision->mat->diffuse(collision->getTexUV(uv));
  case Coordinate::BARY:
    return collision->mat->diffuse(collision->getTexUV(bary));
  default:
    return collision->mat->diffuse();
  }
}

vec3 Ray::collisionSpecularColour(vec3 lightDirection, vec3 lightColour) const {
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
