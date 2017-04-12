#include "convergent_gi.h"

ConvergentGlobalIllumination::ConvergentGlobalIllumination(const Scene &scene,
                                                           int sampleCount,
                                                           int width,
                                                           int height)
    : LightingEngine(scene.triangles, scene.light), gi(scene, sampleCount),
      image(vector<vec3>(width * height)), width(width), height(height){};

vec3 ConvergentGlobalIllumination::calculateLight(Ray &ray, ivec2 pixel) {
  image[width * pixel.y + pixel.x] += gi.calculateLight(ray);
  return image[width * pixel.y + pixel.x] / static_cast<float>(countedSamples);
}
