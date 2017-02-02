#pragma once

#include <limits>
#include <vector>

#include "camera.h"
#include "lerp.h"
#include "sdlscreen.h"
#include "testmodel.h"
#include "pointlight.h"

using std::numeric_limits;
using std::vector;

class RayTracer: public SdlScreen {
private:
	bool ClosestIntersection(Ray &ray, const vector<Triangle> &triangles);
	vector<Triangle> triangles;

    Camera camera;

	const vec3 ambientLight = vec3(0.15f, 0.15f, 0.15f);
	PointLight light;
protected:
	void update(float dt) override;
	void draw(int width, int height) override;

public:
	RayTracer(int width, int height, bool fullscreen = false);
};
