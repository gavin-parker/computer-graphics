#pragma once

#include "material.h"
#include <algorithm>
#include <memory>
#include <vector>
#include <limits>

using std::shared_ptr;
using std::vector;
using std::numeric_limits;

class Cube;

#include "ray.h"

class Cube {
public:
	vec3 a,b;

	Cube(vec3 a, vec3 b);

	Cube(const shared_ptr<const vector<Triangle>> triangles);

	bool calculateIntersection(Ray &ray) const;
};
