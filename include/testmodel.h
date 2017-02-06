#pragma once
// Defines a simple test model: The Cornel Box

#include <iostream>
#include <vector>

#include "lodepng.h"
#include "material.h"
#include "triangle.h"

using std::vector;
using glm::mat3x2;
using glm::vec2;

vector<Triangle> loadTestModel();
