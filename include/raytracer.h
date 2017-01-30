#pragma once

#include <vector>

#include "lerp.h"
#include "sdlscreen.h"
#include "triangle.h"
#include "limits.h"
#include <glm/glm.hpp>

struct Intersection
{
    vec3 position;
    float distance;
    int triangleIndex;
};


class RayTracer: public SdlScreen {
private:
    bool ClosestIntersection(vec3 start, vec3 dir, const vector<Triangle> &triangles, Intersection& closestIntersection);

protected:
	void update(float dt) override;
	void draw() override;

public:
	RayTracer(int width, int height, bool fullscreen = false);
};
