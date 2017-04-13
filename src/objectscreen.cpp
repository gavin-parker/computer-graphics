#include "objectscreen.h"

ObjectScreen::ObjectScreen(int width, int height, float viewAngle,
                           LightingEngine &lighting, Scene &scene,
                           bool fullscreen)
    : SdlScreen(width, height, fullscreen), lighting(lighting),
      camera(scene.bounds, 2.0f, 6.0f, viewAngle), light(scene.light) {}

ObjectScreen::~ObjectScreen() {}

void ObjectScreen::update(float dt) {
  camera.update(dt);
  light.update(dt);
}
