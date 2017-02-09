#include "standardlighting.h"

// GlobalIllumination::GlobalIllumination(){};

StandardLighting::StandardLighting(
    const shared_ptr<const vector<Triangle>> triangles,
    shared_ptr<PointLight> light)
    : LightingEngine(triangles, light){};

vec3 StandardLighting::calculateLight(Ray ray) {
  Ray lightRay;
  light->calculateRay(lightRay, ray.collisionLocation);
  ClosestIntersection(lightRay);

  vec3 lightColour(0, 0, 0);
  shared_ptr<const Material> mat = ray.collision->mat;
  if (lightRay.collision == ray.collision) {

    vec3 n = lightRay.collision->normal;
    vec3 v = glm::normalize(ray.direction);
    vec3 l = glm::normalize(lightRay.direction);
    vec3 spec = mat->phong(v, l, n);

    lightColour =
        light->directLight(ray) * mat->diffuse + spec * mat->specularity;
  } else {
    lightColour = ambientLight * mat->diffuse;
  }

  return ray.collision->getPixelColour(ray.collisionUVLocation) * lightColour;
}

bool StandardLighting::ClosestIntersection(Ray &ray) {
  ray.length = numeric_limits<float>::max();

  bool anyIntersection = false;

  for (const Triangle &triangle : *triangles) {
    anyIntersection |= triangle.calculateIntection(ray);
  }

  return anyIntersection;
}
