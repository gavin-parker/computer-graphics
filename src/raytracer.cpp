#include "raytracer.h"

RayTracer::RayTracer(int width, int height, float viewAngle,
                     unsigned subPixelCount, LightingEngine &lighting,
                     Scene &scene, bool fullscreen, bool antialias)
    : ObjectScreen(width, height, viewAngle, lighting, scene, fullscreen),
      triangles(scene.triangles), boundingVolume(scene.volume),
      antialias(antialias), subPixelCount(subPixelCount) {}

void RayTracer::draw(int width, int height) {

  static int rows_completed = 0;
  static int counter_last = 0;

  int margin_y = height;
  int margin_x = width;
  if (antialias) {
    margin_y--;
    margin_x--;
  }

#pragma omp parallel for
  for (int y = 0; y < margin_y; ++y) {
    for (int x = 0; x < margin_x; ++x) {
      vec3 average(0, 0, 0);
      for (unsigned i = 0; i < subPixelCount; ++i) {
        for (unsigned j = 0; j < subPixelCount; ++j) {
          float super_x =
              static_cast<float>(x) + i / static_cast<float>(subPixelCount);
          float super_y =
              static_cast<float>(y) + j / static_cast<float>(subPixelCount);
          Ray cameraRay =
              camera.calculateRay(super_x / width, super_y / height);

          if (boundingVolume.calculateIntersection(cameraRay, true)) {
            average += lighting.calculateLight(cameraRay, glm::ivec2(x, y));
          }
        }
      }
      average /= subPixelCount * subPixelCount;
      drawPixel(x, y, vec3(std::min(average.r, 1.0f), std::min(average.g, 1.0f),
                           std::min(average.b, 1.0f)));
    }
#pragma omp critical
    {
      rows_completed++;
      int percent_done = static_cast<float>(rows_completed) /
                         static_cast<float>(height) * 100.f;
      if (percent_done % 10 == 0 && percent_done > counter_last) {
        cout << percent_done << "%\n";
        counter_last = percent_done;
      }
    }
  }
  lighting.countedSamples++;
  rows_completed = 0;
  counter_last = 0;
}
