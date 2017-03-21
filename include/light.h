#pragma once

#define _USE_MATH_DEFINES

#include <SDL.h>
#include <algorithm>
#include <cmath>
#include <math.h>

#include "lerp.h"
#include "ray.h"
#include "vertex.h"

#ifndef unix
inline float RAND() { return ((float)rand() / (RAND_MAX)); }
#else
inline float RAND() { return drand48(); }
#endif

typedef struct indexedPixel {
  int x;
  int y;
  int i;
} indexedPixel;

using glm::ivec2;
using glm::vec3;
using std::numeric_limits;
using std::vector;

class Light {
protected:
  const vec3 RIGHT = vec3(1.f, 0.f, 0.f);
  const vec3 LEFT = vec3(-1.f, 0.f, 0.f);
  const vec3 UP = vec3(0.f, 1.f, 0.f);
  const vec3 DOWN = vec3(0.f, -1.f, 0.f);
  const vec3 FORWARD = vec3(0.f, 0.f, 1.f);
  const vec3 BACK = vec3(0.f, 0.f, -1.f);
  const vec3 EMPTY = vec3(0.f, 0.f, 0.f);

  const mat3 rotations[6] = {
      mat3(RIGHT, UP, FORWARD), mat3(LEFT, UP, BACK),
      mat3(BACK, UP, RIGHT),    mat3(FORWARD, UP, LEFT),
      mat3(RIGHT, BACK, UP),    mat3(RIGHT, FORWARD, DOWN)};

public:
  vec3 position;
  vec3 color;
  float power;
  int rayCount = 1;
  int width;
  int height;

  // float power;

  virtual bool update(float dt) = 0;

  virtual vector<Ray> calculateRays(vec3 target) const = 0;

  virtual vec3 directLight(const Ray &ray) const = 0;

  virtual vec3 vertexLight(Vertex v) const = 0;

  Light(vec3 position, vec3 color, float power, int rayCount, int width = 128,
        int height = 128)
      : position(position), color(color), power(power), rayCount(rayCount),
        width(width), height(height){};

  indexedPixel projectVertex(vec3 vert, float &depth) {
    depth = numeric_limits<float>::max();
    for (int i = 0; i < 6; i++) {
      vec3 newPos = (vert - position) * rotations[i];
      float xf = static_cast<float>((newPos.x / newPos.z));
      float yf = static_cast<float>((newPos.y / newPos.z));
      int x = static_cast<int>(width * (1 - xf) / 2.0);
      int y = static_cast<int>(height * (1 - yf) / 2.0);
      if (x >= 0 && x < width && y >= 0 && y < height &&
          newPos.z < numeric_limits<float>::max() && newPos.z > 0.f) {
        depth = newPos.z;
        return {x, y, i};
      }
    }
    return {-1, -1, -1};
  }
};
