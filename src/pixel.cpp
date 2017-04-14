#include "pixel.h"

Pixel::Pixel() : Pixel(0) {}

Pixel::Pixel(int x) : Pixel(x, 0, 0.0f, vec3(0.0f, 0.0f, 0.0f)) {}

Pixel::Pixel(int x, int y, float depth, vec3 position)
    : x(x), y(y), depth(depth), position(position) {}
