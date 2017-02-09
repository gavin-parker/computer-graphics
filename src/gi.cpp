#include "gi.h"

// GlobalIllumination::GlobalIllumination(){};

GlobalIllumination::GlobalIllumination(Scene scene) : LightingEngine(scene){};

vec3 GlobalIllumination::trace(Ray ray, int bounces) {
  // find diffuse light at this position
  float diffuse = ray.collision->mat->diffuse;
  vec3 lightHere(0, 0, 0);
  Ray directLightRay;
  light->calculateRay(directLightRay, ray.collisionLocation);

  if (!anyIntersection(directLightRay, ray)) {
    return vec3(0, 0, 0);
  } else if (directLightRay.collision == ray.collision) {
    lightHere = light->directLight(ray);
  }

  vec3 indirectLight(0, 0, 0);

  // create orthogonal basis on plane
  vec3 normal = ray.collision->normal;
  vec3 normalX = ray.collision->e1;
  vec3 normalY = glm::cross(normal, normalX);

  mat3 basis(normalX, normalY, normal);
  int count = 0;
  for (int i = 0; i < sampleCount; i++) {

    // generate random direction
    float theta = drand48() * M_PI;
    float cosTheta = cos(theta);

    float dist = drand48();
    vec3 direction = glm::normalize(
        vec3(drand48() * 2.f - 1.f, drand48() * 2.f - 1.f, dist));

    if (bounces >= 1) {
      Ray bounce;
      bounce.position = ray.collisionLocation;
      bounce.direction = direction * basis;
      // return this + new collision point
      if (ClosestIntersection(bounce)) {
        count++;
        indirectLight += cosTheta * trace(bounce, bounces - 1);
      }
    } else {
      return lightHere * diffuse;
    }
  }
  return (lightHere + (indirectLight / static_cast<float>(count))) * diffuse /
         static_cast<float>(M_PI);
}

vec3 GlobalIllumination::calculateLight(Ray ray) {
  return ray.collision->getPixelColour(ray.collisionUVLocation) * trace(ray, 3);
}

bool GlobalIllumination::ClosestIntersection(Ray &ray) {
  ray.length = numeric_limits<float>::max();

  bool anyIntersection = false;

  for (const Triangle &triangle : *triangles) {
    anyIntersection |= triangle.calculateIntection(ray);
  }

  return anyIntersection;
}

// like closestIntersection, but backs out after a single intersection
bool GlobalIllumination::anyIntersection(Ray &ray, Ray &surface) {
  bool anyIntersection = false;
  float lightDistance = ray.length;
  ray.length = numeric_limits<float>::max();
  for (const Triangle &triangle : *triangles) {
    anyIntersection |= triangle.calculateIntection(ray);
    if (anyIntersection && ray.collision != surface.collision &&
        ray.length < lightDistance) {
      return anyIntersection;
    }
  }
  return anyIntersection;
}
