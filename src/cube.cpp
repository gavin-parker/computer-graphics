#include "cube.h"

Cube::Cube(vec3 a, vec3 b) : a(a), b(b){};

Cube::Cube(const Ptr_Triangles &triangles) {
  float maxFloat = numeric_limits<float>::max();
  vec3 maxBound(-maxFloat, -maxFloat, -maxFloat);
  vec3 minBound(maxFloat, maxFloat, maxFloat);

  for (const Ptr_Triangle &triangle : triangles) {
    for (const vec3 &vertex : triangle->getVertices()) {
      maxBound.x = std::max(vertex.x, maxBound.x);
      maxBound.y = std::max(vertex.y, maxBound.y);
      maxBound.z = std::max(vertex.z, maxBound.z);
      minBound.x = std::min(vertex.x, minBound.x);
      minBound.y = std::min(vertex.y, minBound.y);
      minBound.z = std::min(vertex.z, minBound.z);
    }
  }

  a = minBound;
  b = maxBound;
};

bool Cube::calculateIntersection(Ray &ray) const {
  float tmin = (a.x - ray.getPosition().x) / ray.getDirection().x;
  float tmax = (b.x - ray.getPosition().x) / ray.getDirection().x;

  if (tmin > tmax) {
    float t = tmin;
    tmin = tmax;
    tmax = t;
  }

  float tymin = (a.y - ray.getPosition().y) / ray.getDirection().y;
  float tymax = (b.y - ray.getPosition().y) / ray.getDirection().y;

  if (tymin > tymax) {
    float t = tymin;
    tymin = tymax;
    tymax = t;
  }
  if ((tmin > tymax) || (tymin > tmax)) {
    return false;
  }
  if (tymin > tmin) {
    tmin = tymin;
  }
  if (tymax < tmax) {
    tmax = tymax;
  }

  float tzmin = (a.z - ray.getPosition().z) / ray.getDirection().z;
  float tzmax = (b.z - ray.getPosition().z) / ray.getDirection().z;

  if (tzmin > tzmax) {
    float t = tzmin;
    tzmin = tzmax;
    tzmax = t;
  }
  if ((tmin > tzmax) || (tzmin > tmax)) {
    return false;
  }
  if (tzmin > tmin) {
    tmin = tzmin;
  }
  if (tzmax < tmax) {
    tmax = tzmax;
  }

  return true;
};
