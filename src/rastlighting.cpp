#include "rastlighting.h"

RastLighting::RastLighting(const Scene &scene)
    : LightingEngine(scene.triangles, scene.light) {}

vec3 RastLighting::calculateLight(Ray &cameraRay, ivec2 pixel) {
  vec3 lightColour = vec3(0.0f, 0.0f, 0.0f);

  for (Ray &lightRay : light.calculateRays(cameraRay.collisionLocation())) {
    lightRay.updateCollision(cameraRay);
    lightColour += light.directLight(lightRay) *
                   (cameraRay.collisionDiffuseColour(lightRay.getDirection()) +
                    cameraRay.collisionSpecularColour(lightRay.getDirection()));
  }
  return lightColour;
}
