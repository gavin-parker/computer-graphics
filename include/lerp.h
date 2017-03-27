#pragma once

#include "pixel.h"

// Linearly interpolate between a and b
float lerpF(float a, float b, float t);

float deLerpF(float a, float b, float t);

float deLerp(vec3 a, vec3 b, float t);

vec3 lerp(vec3 a, vec3 b, float t);

int lerpI(int a, int b, float t);

Vertex lerpV(Vertex a, Vertex b, float a_z, float b_z, float c_z, float t);

Pixel lerpP(Pixel a, Pixel b, float t);
