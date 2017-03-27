#include "standardlighting.h"

// GlobalIllumination::GlobalIllumination(){};

StandardLighting::StandardLighting(const Scene &scene)
    : LightingEngine(scene.triangles, scene.light),
      boundingVolume(scene.volume){};

vec3 StandardLighting::calculateLight(Ray &ray, ivec2 pixel) {
  // ClosestIntersection(lightRay);

  vec3 lightColour(0, 0, 0);
  vector<Ray> rays = light.calculateRays(ray.collisionLocation());
  //calculate average light at a point -- works with multiple li ght rays
  for (int i = 0; (size_t)i < rays.size(); i++) {
	  Ray lightRay = rays[i];

	  if (boundingVolume.calculateAnyIntersection(lightRay, ray, true) && sametriangle(lightRay.getCollision(), ray.getCollision())) {

      lightColour += light.directLight(ray) * ray.collisionDiffuseColour() +
                     ray.collisionSpecularColour(lightRay.getDirection(),
                                                 vec3(1.0f, 1.0f, 1.0f));
    } else {
      lightColour += ambientLight * ray.collisionDiffuseColour();
    }
  }
  lightColour /= rays.size();
  return ray.collisionDiffuseColour() * lightColour;
}

bool StandardLighting::ClosestIntersection(Ray &ray) {
  ray.extendToInfinity();

  bool anyIntersection = false;

  for (const Triangle &triangle : triangles) {
    anyIntersection |= triangle.calculateIntersection(ray);
  }

  return anyIntersection;
}

// like closestIntersection, but backs out after a single intersection
bool StandardLighting::anyIntersection(Ray &ray, Ray &surface) {
  bool anyIntersection = false;
  float lightDistance = ray.getLength();
  ray.extendToInfinity();
  for (const Triangle &triangle : triangles) {
    anyIntersection |= triangle.calculateIntersection(ray);
    if (anyIntersection && ray.getCollision() != surface.getCollision() &&
        ray.getLength() < lightDistance) {
      return anyIntersection;
    }
  }
  return anyIntersection;
}
