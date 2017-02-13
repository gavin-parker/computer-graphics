#pragma once
#define _USE_MATH_DEFINES
#include <cmath>
#include <SDL.h>
#include <algorithm>
#include <math.h>

#include "lerp.h"
#include "ray.h"
#include "vertex.h"

class PointLight {
private:
  vec3 color;
  float power;

  const float velocity = 200.0f;

public:
	vec3 position;

  PointLight(vec3 position, vec3 color, float power);

  void update(float dt);

  void calculateRay(Ray &ray, vec3 target) const;

  vec3 directLight(const Ray &ray) const;

  vec3 vertexLight(Vertex v) const;
};
