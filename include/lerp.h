#pragma once
#include <glm/glm.hpp>
#include <vector>

using namespace std;
using glm::vec3;

// Linearly interpolate between a and b
float lerp(float a, float b, float t);

// Generate a linear interpolation between a and b
void lerp(float a, float b, std::vector<float>& results);

float deLerp(float a, float b, float t);
