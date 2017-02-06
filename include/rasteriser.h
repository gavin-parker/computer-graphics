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

class Rasteriser : public SdlScreen {
private:
  vector<Triangle> triangles;
  Camera camera;

  void drawPolygonEdges(const vector<vec3> &vertices);
  void drawEdge(vec2 a, vec2 b, vec3 color);

protected:
  void update(float dt) override;
  void draw(int width, int height) override;

public:
  Rasteriser(int width, int height, bool fullscreen = false);
};
