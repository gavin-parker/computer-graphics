#pragma once
// Defines a simple test model: The Cornel Box

#include <vector>
#include <iostream>

#include "triangle.h"
#include "textureloader.h"

using std::vector;
using glm::mat3x2;
using glm::vec2;
vector<Triangle> loadTestModel();

