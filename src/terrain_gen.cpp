#include "terrain_gen.h"



const shared_ptr<const vector<Triangle>> TerrainGenerator::makePlane(float width, int resolution, vec3 pos) {
	shared_ptr<vector<Triangle>> triangles(new vector<Triangle>());
	shared_ptr<Material> marble(new Material(50, 0.8f, 0.7f));
	marble->loadPNG("marble.png");
	vec2 bl(0.0f, 1.0f);
	vec2 br(1.0f, 1.0f);
	vec2 tl(0.0f, 0.0f);
	vec2 tr(1.0f, 0.0f);
	vec3 white(0.75f, 0.75f, 0.75f);
	float squareWidth = width / static_cast<float>(resolution);
	for (int x = 0; x < resolution; x++) {
		for (int z = 0; z < resolution; z++) {
			vec3 v1(squareWidth * x, 0, squareWidth * z);
			vec3 v2(squareWidth * (x+1), 0, squareWidth * z);
			vec3 v3(squareWidth * x, 0, squareWidth * (z + 1));
			vec3 v4(squareWidth * (x + 1), 0, squareWidth * (z + 1));
			triangles->push_back(Triangle(v1+pos, v2 + pos, v3 + pos, tl, br, bl, white, marble));
			triangles->push_back(Triangle(v4 + pos, v3 + pos, v2 + pos, tl, tr, br, white, marble));
		}
	}
	return triangles;
}

const shared_ptr<const vector<Triangle>> TerrainGenerator::generateTerrain(float width, int maxHeight, int resolution, vec3 pos) {
	return makePlane(width, resolution, pos);
}