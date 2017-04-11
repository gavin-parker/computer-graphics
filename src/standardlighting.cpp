#include "standardlighting.h"

StandardLighting::StandardLighting(const Scene &scene)
    : LightingEngine(scene.triangles, scene.light),
      boundingVolume(scene.volume){};

vec3 StandardLighting::calculateLight(Ray &cameraRay, ivec2 pixel) {
  vec3 lightColour = cameraRay.collisionAmbientColour(ambientColour);

  // calculate average light at a point -- works with multiple light rays
  for (Ray &lightRay : light.calculateRays(cameraRay.collisionLocation())) {
    if (boundingVolume.calculateAnyIntersection(lightRay, cameraRay, true) &&
        lightRay.getCollision() == cameraRay.getCollision()) {

      lightColour +=
          light.directLight(cameraRay) * cameraRay.collisionDiffuseColour() +
          ray.collisionSpecularColour(lightRay.getDirection(),
                                      vec3(1.0f, 1.0f, 1.0f));
    } else {
      lightColour += ambientLight * cameraRay.collisionDiffuseColour();
    }
  }
  return ray.collisionDiffuseColour() * lightColour;
}

bool StandardLighting::ClosestIntersection(Ray &ray) {
  ray.extendToInfinity();

  bool anyIntersection = false;

  for (const Ptr_Triangle &triangle : triangles) {
    anyIntersection |= triangle->calculateIntersection(ray);
  }

  return anyIntersection;
}

// like closestIntersection, but backs out after a single intersection
bool StandardLighting::anyIntersection(Ray &ray, Ray &surface) {
  bool anyIntersection = false;
  float lightDistance = ray.getLength();
  ray.extendToInfinity();
  for (const Ptr_Triangle &triangle : triangles) {
    anyIntersection |= triangle->calculateIntersection(ray);
    if (anyIntersection && ray.getCollision() != surface.getCollision() &&
        ray.getLength() < lightDistance) {
      return anyIntersection;
    }
  }
  return anyIntersection;
}
