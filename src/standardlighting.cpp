#include "standardlighting.h"

StandardLighting::StandardLighting(const Scene &scene)
    : LightingEngine(scene.triangles, scene.light),
      boundingVolume(scene.volume){};

vec3 StandardLighting::calculateLight(Ray &cameraRay, ivec2 pixel) {
  vec3 lightColour = ambientLight * cameraRay.collisionAmbientColour();

  // calculate average light at a point -- works with multiple light rays
  for (Ray &lightRay : light.calculateRays(cameraRay.collisionLocation())) {
    if (boundingVolume.calculateAnyIntersection(lightRay, cameraRay, false) &&
        lightRay.getCollision() == cameraRay.getCollision()) {

      lightColour +=
          light.directLight(lightRay) *
          (cameraRay.collisionDiffuseColour(lightRay.getDirection()) +
           cameraRay.collisionSpecularColour(lightRay.getDirection()));
    }
  }
  return lightColour;
}
