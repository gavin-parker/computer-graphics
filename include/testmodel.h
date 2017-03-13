#pragma once
// Defines a simple test model: The Cornel Box
#include "lodepng.h"
#include "triangle.h"
#include "scene.h"
#include "bvh.h"


shared_ptr<vector<Triangle>> loadTestModel();

const shared_ptr<BoundingVolume> loadTestModelBVH(); 