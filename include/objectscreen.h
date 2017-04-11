#pragma once

#include "camera.h"
#include "lightingengine.h"
#include "sdlscreen.h"

class ObjectScreen : public SdlScreen {
protected:
  LightingEngine &lighting;
  Camera camera;
  Light &light;

  vec3 ambientLight;

  ObjectScreen(int width, int height, float viewAngle, LightingEngine &lighting,
               Scene &scene, bool fullscreen);

  virtual ~ObjectScreen();

  void update(float dt) override;
};
