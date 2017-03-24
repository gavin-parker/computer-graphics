#pragma once

#include "vertex.h"

class Pixel {
public:
  int x, y;
  float depth;
  Vertex v;
  Pixel() : x(0), y(0), depth(0.0f) {}
  Pixel(int x, int y, float depth) : x(x), y(y), depth(depth) {}
  Pixel(int x, int y, float depth, Vertex v) : x(x), y(y), depth(depth), v(v) {}
};
