#pragma once

#define _USE_MATH_DEFINES

#include <algorithm>

#include "lerp.h"
#include "myrand.h"
#include "ray.h"

struct indexedPixel {
  int x;
  int y;
  int i;
};

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
  const vec3 colour;
  float power;
  int rayCount = 1;
  int width;
  int height;

  Light(vec3 position, vec3 colour, float power, int rayCount, int width = 128,
        int height = 128);

  virtual bool update(float dt) = 0;

  virtual vector<Ray> calculateRays(vec3 target) const = 0;

  vec3 directLight(const Ray &ray) const;

  indexedPixel projectVertex(vec3 vert, float &depth);
};
