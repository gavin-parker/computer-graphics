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
        if (lightRay.collision == cameraRay.collision) {

          vec3 n = cameraRay.collision->normal;
          vec3 v = glm::normalize(cameraRay.direction);
          vec3 l = glm::normalize(lightRay.direction);
          shared_ptr<const Material> mat = cameraRay.collision->mat;
          vec3 spec = mat->phong(v, l, n);
          lightColour = light.directLight(cameraRay) * mat->diffuse +
                        spec * mat->specularity;
        }

        vec3 rayColour =
            cameraRay.collision->getColour(cameraRay.collisionUVLocation);
        drawPixel(x, y, vec3(std::min(rayColour.r * lightColour.r, 1.0f),
                             std::min(rayColour.g * lightColour.g, 1.0f),
                             std::min(rayColour.b * lightColour.b, 1.0f)));
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
