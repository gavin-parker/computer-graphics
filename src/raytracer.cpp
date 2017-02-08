#include "raytracer.h"

RayTracer::RayTracer(int width, int height, bool fullscreen)
    : SdlScreen(width, height, fullscreen), triangles(loadTestModel()),
      camera(vec3(277.5f, 277.5f, -480.64), 0.0f, 30.0f),
      light(vec3(400.0f, 100.0f, 100.0f), vec3(1.0, 1.0f, 1.0f), 500000.0f) {}

void RayTracer::update(float dt) {
  light.update(dt);
  camera.update(dt);
}

void RayTracer::draw(int width, int height) {
#pragma omp parallel for
  for (int y = 0; y < height; ++y) {
    for (int x = 0; x < width; ++x) {
      Ray cameraRay, lightRay;

      camera.calculateRay(cameraRay, static_cast<float>(x) / width,
                          static_cast<float>(y) / height);

      if (ClosestIntersection(cameraRay)) {
        light.calculateRay(lightRay, cameraRay.collisionLocation);
        ClosestIntersection(lightRay);

        vec3 lightColour(0, 0, 0);
        shared_ptr<const Material> mat = cameraRay.collision->mat;
        if (lightRay.collision == cameraRay.collision) {

          vec3 n = lightRay.collision->normal;
          vec3 v = glm::normalize(cameraRay.direction);
          vec3 l = glm::normalize(lightRay.direction);
          vec3 spec = mat->phong(v, l, n);

          lightColour = light.directLight(cameraRay) * mat->diffuse +
                        spec * mat->specularity;
        } else {
          lightColour = ambientLight * mat->diffuse;
        }

        vec3 surfaceColour =
            cameraRay.collision->getPixelColour(cameraRay.collisionUVLocation);

        drawPixel(x, y, vec3(std::min(surfaceColour.r * lightColour.r, 1.0f),
                             std::min(surfaceColour.g * lightColour.g, 1.0f),
                             std::min(surfaceColour.b * lightColour.b, 1.0f)));
      }
    }
  }
}

bool RayTracer::ClosestIntersection(Ray &ray) {
  ray.length = numeric_limits<float>::max();

  bool anyIntersection = false;

  for (const Triangle &triangle : *triangles) {
    anyIntersection |= triangle.calculateIntection(ray);
  }

  return anyIntersection;
}

vec3 RayTracer::globalIllumination(const Ray ray, int bounces) {
  // find diffuse light at this position
  if (bounces > 0) {
    float diffuse = ray.collision->mat->diffuse;
    vec3 lightHere = ray.collision->getPixelColour(ray.collisionUVLocation);
    Ray directLightRay;
    light.calculateRay(directLightRay, ray.collisionLocation);
    ClosestIntersection(directLightRay);
    if (directLightRay.collision == ray.collision) {
      lightHere *= light.directLight(ray) * diffuse;
    }
    // generate random direction
    // float rand = drand48() * M_PI;

    return lightHere;
    // return this + new collision point
  }
  return vec3(0, 0, 0);
}
