#include "rasteriser.h"

Rasteriser::Rasteriser(int width, int height, bool fullscreen)
    : SdlScreen(width, height, fullscreen),
      camera(vec3(277.5f, 277.5f, -480.64), static_cast<float>(M_PI), 30.0f) {
  triangles = loadTestModel();
}

void Rasteriser::update(float dt) { camera.update(dt); }

void Rasteriser::draw(int width, int height) {}
