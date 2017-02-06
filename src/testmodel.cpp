#include "testmodel.h"

vector<Triangle> loadTestModel() {
  vector<Triangle> triangles;

  // Material tiles, doggo;
  Material doggo;

  // tiles.loadPNG("tiles.png");
  doggo.loadPNG("texture.png");

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

  triangles.clear();
  triangles.reserve(5 * 2 * 3);

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
  triangles.push_back(Triangle(C, B, A, tl, br, bl, green, doggo));
  triangles.push_back(Triangle(C, D, B, tl, tr, br, green, doggo));

  // Left wall
  triangles.push_back(Triangle(A, E, C, bl, tl, br, purple, doggo));
  triangles.push_back(Triangle(C, E, G, br, tl, tr, purple, doggo));

  // Right wall
  triangles.push_back(Triangle(F, B, D, tr, br, bl, yellow, doggo));
  triangles.push_back(Triangle(H, F, D, tl, tr, bl, yellow, doggo));

  // Ceiling
  triangles.push_back(Triangle(E, F, G, tl, tr, bl, cyan, doggo));
  triangles.push_back(Triangle(F, H, G, tr, br, bl, cyan, doggo));

  // Back wall
  triangles.push_back(Triangle(G, D, C, tl, br, bl, white, doggo));
  triangles.push_back(Triangle(G, H, D, tl, tr, br, white, doggo));

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
  triangles.push_back(Triangle(E, B, A, tl, br, bl, red, doggo));
  triangles.push_back(Triangle(E, F, B, tl, tr, br, red, doggo));

  // RIGHT
  triangles.push_back(Triangle(F, D, B, tl, br, bl, red, doggo));
  triangles.push_back(Triangle(F, H, D, tl, tr, br, red, doggo));

  // BACK
  triangles.push_back(Triangle(H, C, D, tl, br, bl, red, doggo));
  triangles.push_back(Triangle(H, G, C, tl, tr, br, red, doggo));

  // LEFT
  triangles.push_back(Triangle(G, E, C, tl, tr, bl, red, doggo));
  triangles.push_back(Triangle(E, A, C, tr, br, bl, red, doggo));

  // TOP
  triangles.push_back(Triangle(G, F, E, tl, br, bl, red, doggo));
  triangles.push_back(Triangle(G, H, F, tl, tr, br, red, doggo));

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
  triangles.push_back(Triangle(E, B, A, tl, br, bl, blue, doggo));
  triangles.push_back(Triangle(E, F, B, tl, tr, br, blue, doggo));

  // RIGHT
  triangles.push_back(Triangle(F, D, B, tl, br, bl, blue, doggo));
  triangles.push_back(Triangle(F, H, D, tl, tr, br, blue, doggo));

  // BACK
  triangles.push_back(Triangle(H, C, D, tl, br, bl, blue, doggo));
  triangles.push_back(Triangle(H, G, C, tl, tr, br, blue, doggo));

  // LEFT
  triangles.push_back(Triangle(G, E, C, tl, tr, bl, blue, doggo));
  triangles.push_back(Triangle(E, A, C, tr, br, bl, blue, doggo));

  // TOP
  triangles.push_back(Triangle(G, F, E, tl, br, bl, blue, doggo));
  triangles.push_back(Triangle(G, H, F, tl, tr, br, blue, doggo));

  return triangles;
}
