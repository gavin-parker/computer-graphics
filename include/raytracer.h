#pragma once

#include <limits>
#include <vector>

#include "camera.h"
#include "lerp.h"
#include "sdlscreen.h"
#include "testmodel.h"

using std::numeric_limits;
using std::vector;

struct Intersection
{
	vec3 position;
	float distance;
	Triangle const *triangle;
};

class RayTracer: public SdlScreen {
private:
	bool ClosestIntersection(Ray ray, const vector<Triangle> &triangles, Intersection& closestIntersection);
	vector<Triangle> triangles;
    Camera camera;
protected:
	void update(float dt) override;
	void draw(int width, int height) override;

public:
	RayTracer(int width, int height, bool fullscreen = false);
};
