#include "flatlighting.h"

// GlobalIllumination::GlobalIllumination(){};

FlatLighting::FlatLighting(Scene &scene)
    : LightingEngine(scene.triangles, scene.light),
      boundingVolume(scene.volume){};

vec3 FlatLighting::calculateLight(Ray ray, ivec2 pixel) {
  return ray.collisionDiffuseColour();
}

bool FlatLighting::ClosestIntersection(Ray &ray) {
  bool anyIntersection = false;

  for (const Ptr_Triangle &triangle : triangles) {
    anyIntersection |= triangle->calculateIntersection(ray);
  }

  return anyIntersection;
}

// like closestIntersection, but backs out after a single intersection
bool FlatLighting::anyIntersection(Ray &ray, Ray &surface) {
  bool anyIntersection = false;
  float lightDistance = ray.getLength();
  for (const Ptr_Triangle &triangle : triangles) {
    anyIntersection |= triangle->calculateIntersection(ray);
    if (anyIntersection && ray.getCollision() != surface.getCollision() &&
        ray.getLength() < lightDistance) {
      return anyIntersection;
    }
  }
  return anyIntersection;
}
