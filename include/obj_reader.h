#pragma once
// Loads a 2 sided obj
#include "lodepng.h"
#include "triangle.h"
#include "scene.h"
#include "bvh.h"
#include <fstream>
using namespace std;


const shared_ptr<const vector<Triangle>> loadObj(string filename, float scale_factor = 30.f, bool inCornell = true);
