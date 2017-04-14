#pragma once

#include <glm/glm.hpp>

#include "pixel.h"

// Linearly interpolate between a and b
float lerpF(float a, float b, float t);

float deLerpF(float a, float b, float t);

int lerpI(int a, int b, float t);

float deLerpI(int a, int b, int t);

vec3 lerpV(vec3 a, vec3 b, float t);

vec3 lerpV(vec3 a, vec3 b, vec3 t);

Pixel lerpP(Pixel a, Pixel b, float t);
