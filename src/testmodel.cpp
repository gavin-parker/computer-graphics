#include "testmodel.h"

const shared_ptr<const vector<Triangle>> loadTestModel() {
  shared_ptr<vector<Triangle>> triangles(new vector<Triangle>());

  // Material tiles, green;
  shared_ptr<Material> greenMat(new Material(2, 0.04f, 0.7f));
  shared_ptr<Material> marble(new Material(50, 0.8f, 0.7f));
  shared_ptr<Material> pink(new Material(2, 0.04f, 0.7f));

  // tiles.loadPNG("tiles.png");
  greenMat->loadPNG("green.png");
  pink->loadPNG("red.png");

  marble->loadPNG("marble.png");

  // Defines colors:
  vec3 red(0.75f, 0.15f, 0.15f);
  vec3 yellow(0.75f, 0.75f, 0.15f);
  vec3 green(0.15f, 0.75f, 0.15f);
  vec3 cyan(0.15f, 0.75f, 0.75f);
  vec3 blue(0.15f, 0.15f, 0.75f);
  vec3 purple(0.75f, 0.15f, 0.75f);
  vec3 white(0.75f, 0.75f, 0.75f);

  vec2 bl(0.0f, 1.0f);
  vec2 br(1.0f, 1.0f);
  vec2 tl(0.0f, 0.0f);
  vec2 tr(1.0f, 0.0f);

  triangles->clear();
  triangles->reserve(5 * 2 * 3);

  // ---------------------------------------------------------------------------
  // Room

  float L = 555; // Length of Cornell Box side.

  vec3 A(L, 0, 0);
  vec3 B(0, 0, 0);
  vec3 C(L, 0, L);
  vec3 D(0, 0, L);

  vec3 E(L, L, 0);
  vec3 F(0, L, 0);
  vec3 G(L, L, L);
  vec3 H(0, L, L);

  // UV ordering = corner, width, height

  // Floor:
  triangles->push_back(Triangle(C, B, A, tl, br, bl, green, marble));
  triangles->push_back(Triangle(C, D, B, tl, tr, br, green, marble));

  // Left wall
  triangles->push_back(Triangle(A, E, C, bl, tl, br, purple, marble));
  triangles->push_back(Triangle(C, E, G, br, tl, tr, purple, marble));

  // Right wall
  triangles->push_back(Triangle(F, B, D, tr, br, bl, yellow, marble));
  triangles->push_back(Triangle(H, F, D, tl, tr, bl, yellow, marble));

  // Ceiling
  triangles->push_back(Triangle(E, F, G, tl, tr, bl, cyan, marble));
  triangles->push_back(Triangle(F, H, G, tr, br, bl, cyan, marble));

  // Back wall
  triangles->push_back(Triangle(G, D, C, tl, br, bl, white, marble));
  triangles->push_back(Triangle(G, H, D, tl, tr, br, white, marble));

  // ---------------------------------------------------------------------------
  // Short block

  A = vec3(290, 0, 114);
  B = vec3(130, 0, 65);
  C = vec3(240, 0, 272);
  D = vec3(82, 0, 225);

  E = vec3(290, 165, 114);
  F = vec3(130, 165, 65);
  G = vec3(240, 165, 272);
  H = vec3(82, 165, 225);

  // FRONT
  triangles->push_back(Triangle(E, B, A, tl, br, bl, red, pink));
  triangles->push_back(Triangle(E, F, B, tl, tr, br, red, pink));

  // RIGHT
  triangles->push_back(Triangle(F, D, B, tl, br, bl, red, pink));
  triangles->push_back(Triangle(F, H, D, tl, tr, br, red, pink));

  // BACK
  triangles->push_back(Triangle(H, C, D, tl, br, bl, red, pink));
  triangles->push_back(Triangle(H, G, C, tl, tr, br, red, pink));

  // LEFT
  triangles->push_back(Triangle(G, E, C, tl, tr, bl, red, pink));
  triangles->push_back(Triangle(E, A, C, tr, br, bl, red, pink));

  // TOP
  triangles->push_back(Triangle(G, F, E, tl, br, bl, red, pink));
  triangles->push_back(Triangle(G, H, F, tl, tr, br, red, pink));

  // ---------------------------------------------------------------------------
  // Tall block

  A = vec3(423, 0, 247);
  B = vec3(265, 0, 296);
  C = vec3(472, 0, 406);
  D = vec3(314, 0, 456);

  E = vec3(423, 330, 247);
  F = vec3(265, 330, 296);
  G = vec3(472, 330, 406);
  H = vec3(314, 330, 456);

  // FRONT
  triangles->push_back(Triangle(E, B, A, tl, br, bl, blue, greenMat));
  triangles->push_back(Triangle(E, F, B, tl, tr, br, blue, greenMat));

  // RIGHT
  triangles->push_back(Triangle(F, D, B, tl, br, bl, blue, greenMat));
  triangles->push_back(Triangle(F, H, D, tl, tr, br, blue, greenMat));

  // BACK
  triangles->push_back(Triangle(H, C, D, tl, br, bl, blue, greenMat));
  triangles->push_back(Triangle(H, G, C, tl, tr, br, blue, greenMat));

  // LEFT
  triangles->push_back(Triangle(G, E, C, tl, tr, bl, blue, greenMat));
  triangles->push_back(Triangle(E, A, C, tr, br, bl, blue, greenMat));

  // TOP
  triangles->push_back(Triangle(G, F, E, tl, br, bl, blue, greenMat));
  triangles->push_back(Triangle(G, H, F, tl, tr, br, blue, greenMat));

  return triangles;
}

const shared_ptr<BoundingVolume> loadTestModelBVH() {

	shared_ptr<vector<Triangle>> walls(new vector<Triangle>());
	shared_ptr<vector<Triangle>> small_block(new vector<Triangle>());
	shared_ptr<vector<Triangle>> tall_block(new vector<Triangle>());

	// Material tiles, green;
	shared_ptr<Material> greenMat(new Material(2, 0.04f, 0.7f));
	shared_ptr<Material> marble(new Material(50, 0.8f, 0.7f));
	shared_ptr<Material> pink(new Material(2, 0.04f, 0.7f));

	// tiles.loadPNG("tiles.png");
	greenMat->loadPNG("green.png");
	pink->loadPNG("red.png");

	marble->loadPNG("marble.png");

	// Defines colors:
	vec3 red(0.75f, 0.15f, 0.15f);
	vec3 yellow(0.75f, 0.75f, 0.15f);
	vec3 green(0.15f, 0.75f, 0.15f);
	vec3 cyan(0.15f, 0.75f, 0.75f);
	vec3 blue(0.15f, 0.15f, 0.75f);
	vec3 purple(0.75f, 0.15f, 0.75f);
	vec3 white(0.75f, 0.75f, 0.75f);

	vec2 bl(0.0f, 1.0f);
	vec2 br(1.0f, 1.0f);
	vec2 tl(0.0f, 0.0f);
	vec2 tr(1.0f, 0.0f);

	walls->clear();

	// ---------------------------------------------------------------------------
	// Room

	float L = 555; // Length of Cornell Box side.

	vec3 A(L, 0, 0);
	vec3 B(0, 0, 0);
	vec3 C(L, 0, L);
	vec3 D(0, 0, L);

	vec3 E(L, L, 0);
	vec3 F(0, L, 0);
	vec3 G(L, L, L);
	vec3 H(0, L, L);

	// UV ordering = corner, width, height

	// Floor:
	walls->push_back(Triangle(C, B, A, tl, br, bl, green, marble));
	walls->push_back(Triangle(C, D, B, tl, tr, br, green, marble));

	// Left wall
	walls->push_back(Triangle(A, E, C, bl, tl, br, purple, marble));
	walls->push_back(Triangle(C, E, G, br, tl, tr, purple, marble));

	// Right wall
	walls->push_back(Triangle(F, B, D, tr, br, bl, yellow, marble));
	walls->push_back(Triangle(H, F, D, tl, tr, bl, yellow, marble));

	// Ceiling
	walls->push_back(Triangle(E, F, G, tl, tr, bl, cyan, marble));
	walls->push_back(Triangle(F, H, G, tr, br, bl, cyan, marble));

	// Back wall
	walls->push_back(Triangle(G, D, C, tl, br, bl, white, marble));
	walls->push_back(Triangle(G, H, D, tl, tr, br, white, marble));


	shared_ptr<BoundingVolume> wallsVolume(new BoundingVolume(walls));


	// ---------------------------------------------------------------------------
	// Short block

	A = vec3(290, 0, 114);
	B = vec3(130, 0, 65);
	C = vec3(240, 0, 272);
	D = vec3(82, 0, 225);

	E = vec3(290, 165, 114);
	F = vec3(130, 165, 65);
	G = vec3(240, 165, 272);
	H = vec3(82, 165, 225);

	// FRONT
	small_block->push_back(Triangle(E, B, A, tl, br, bl, red, pink));
	small_block->push_back(Triangle(E, F, B, tl, tr, br, red, pink));

	// RIGHT
	small_block->push_back(Triangle(F, D, B, tl, br, bl, red, pink));
	small_block->push_back(Triangle(F, H, D, tl, tr, br, red, pink));

	// BACK
	small_block->push_back(Triangle(H, C, D, tl, br, bl, red, pink));
	small_block->push_back(Triangle(H, G, C, tl, tr, br, red, pink));

	// LEFT
	small_block->push_back(Triangle(G, E, C, tl, tr, bl, red, pink));
	small_block->push_back(Triangle(E, A, C, tr, br, bl, red, pink));

	// TOP
	small_block->push_back(Triangle(G, F, E, tl, br, bl, red, pink));
	small_block->push_back(Triangle(G, H, F, tl, tr, br, red, pink));

	BoundingVolume smallVolume(small_block);


	// ---------------------------------------------------------------------------
	// Tall block

	A = vec3(423, 0, 247);
	B = vec3(265, 0, 296);
	C = vec3(472, 0, 406);
	D = vec3(314, 0, 456);

	E = vec3(423, 330, 247);
	F = vec3(265, 330, 296);
	G = vec3(472, 330, 406);
	H = vec3(314, 330, 456);

	// FRONT
	tall_block->push_back(Triangle(E, B, A, tl, br, bl, blue, greenMat));
	tall_block->push_back(Triangle(E, F, B, tl, tr, br, blue, greenMat));

	// RIGHT
	tall_block->push_back(Triangle(F, D, B, tl, br, bl, blue, greenMat));
	tall_block->push_back(Triangle(F, H, D, tl, tr, br, blue, greenMat));

	// BACK
	tall_block->push_back(Triangle(H, C, D, tl, br, bl, blue, greenMat));
	tall_block->push_back(Triangle(H, G, C, tl, tr, br, blue, greenMat));

	// LEFT
	tall_block->push_back(Triangle(G, E, C, tl, tr, bl, blue, greenMat));
	tall_block->push_back(Triangle(E, A, C, tr, br, bl, blue, greenMat));

	// TOP
	tall_block->push_back(Triangle(G, F, E, tl, br, bl, blue, greenMat));
	tall_block->push_back(Triangle(G, H, F, tl, tr, br, blue, greenMat));

	BoundingVolume tallVolume(tall_block);

	wallsVolume->setSubVolume(tallVolume);
	wallsVolume->setSubVolume(smallVolume);

	return wallsVolume;
}