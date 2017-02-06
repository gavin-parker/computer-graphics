#pragma once

class Pixel {
public:
  int x, y;
  float depth;

  Pixel() : x(0), y(0), depth(0.0f) {}
  Pixel(int x, int y, float depth) : x(x), y(y), depth(depth) {}
};
