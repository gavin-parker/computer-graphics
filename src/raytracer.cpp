#include "raytracer.h"

RayTracer::RayTracer(int width, int height, shared_ptr<LightingEngine> lighting,
                     shared_ptr<PointLight> light,
                     const shared_ptr<const vector<Triangle>> triangles,
                     bool fullscreen)
    : SdlScreen(width, height, fullscreen), triangles(triangles),
      camera(vec3(277.5f, 277.5f, -480.64), 0.0f, 30.0f), light(light),
      lighting(lighting) {}

void RayTracer::update(float dt) {
  light->update(dt);
  camera.update(dt);
}

void RayTracer::draw(int width, int height) {
#pragma omp parallel for
  for (int y = 0; y < height; ++y) {
    for (int x = 0; x < width; ++x) {

      Ray cameraRay;

      camera.calculateRay(cameraRay, static_cast<float>(x) / width,
                          static_cast<float>(y) / height);

      if (ClosestIntersection(cameraRay)) {
        shared_ptr<const Material> mat = cameraRay.collision->mat;

        vec3 spec(0, 0, 0);

        vec3 lightColour = lighting->calculateLight(cameraRay) * mat->diffuse +
                           spec * mat->specularity;

        drawPixel(x, y, vec3(std::min(lightColour.r, 1.0f),
                             std::min(lightColour.g, 1.0f),
                             std::min(lightColour.b, 1.0f)));
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
