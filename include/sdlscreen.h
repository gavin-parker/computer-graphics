#pragma once

#define _USE_MATH_DEFINES

#include <SDL.h>
#include <cmath>
#include <glm/glm.hpp>
#include <iomanip>
#include <iostream>

using glm::vec3;
using std::cout;
using std::endl;
using std::setfill;
using std::setw;

class SdlScreen {
private:
  SDL_Surface *surface;
  Uint32 time;

  bool noQuitMessageSDL();

protected:
  void drawPixel(int x, int y, vec3 colour);

  virtual void update(float dt) = 0;
  virtual void draw(int width, int height) = 0;

public:
  SdlScreen(int width, int height, bool fullscreen = false);

  virtual ~SdlScreen();

  void run();

  void saveBMP(const char *fileName);
};
