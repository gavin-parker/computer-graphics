#pragma once

#include "camera.h"
#include "lerp.h"
#include "pointlight.h"
#include "sdlscreen.h"
#include "testmodel.h"
#include <limits>
#include <omp.h>
#include <vector>

using std::numeric_limits;
using std::vector;
using std::max;
using glm::ivec2;

class Rasteriser : public SdlScreen {
private:
  vector<Triangle> triangles;
  Camera camera;

  void drawEdge(vec2 a, vec2 b, vec3 color);
  void computePolygonRows(const vector<ivec2> &vertexPixels,
                          vector<ivec2> &leftPixels,
                          vector<ivec2> &rightPixels);
  void drawPolygonRows(vector<ivec2> &leftPixels, vector<ivec2> &rightPixels,
                       vec3 color);

protected:
  void update(float dt) override;
  void draw(int width, int height) override;

public:
  Rasteriser(int width, int height, bool fullscreen = false);
};
