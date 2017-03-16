#pragma once
#include "triangle.h"
#include "scene.h"
class TerrainGenerator {
private:
	const shared_ptr<const vector<Triangle>> makePlane(float width, int resolution, vec3 pos);


public:
	const shared_ptr<const vector<Triangle>> generateTerrain(float width, int maxHeight, int resolution, vec3 pos);

};