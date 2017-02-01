#pragma once

#include <vector>
#include <glm/glm.hpp>

#include "lerp.h"
#include "sdlscreen.h"
#include "limits.h"
#include "triangle.h"
#include "TestModel.h"

struct Intersection
{
    vec3 position;
    float distance;
    int triangleIndex;
};

struct Camera
{
    vec3 position;
    glm::mat3 rotation;
    float focal_length;
};

class RayTracer: public SdlScreen {
private:
    bool ClosestIntersection(vec3 start, vec3 dir, const vector<Triangle> &triangles, Intersection& closestIntersection);
    vector<Triangle> triangles;
    Camera camera;
protected:
	void update(float dt) override;
	void draw() override;

public:
	RayTracer(int width, int height, bool fullscreen = false);
};
