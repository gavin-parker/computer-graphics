#include "flatlighting.h"

FlatLighting::FlatLighting(Scene &scene)
    : LightingEngine(scene.triangles, scene.light) {}

vec3 FlatLighting::calculateLight(Ray &cameraRay, ivec2 pixel) {
  return cameraRay.collisionDiffuseColour();
}
