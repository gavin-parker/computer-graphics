#pragma once

#include "camera.h"
#include "pointlight.h"
#include "sdlscreen.h"
#include "testmodel.h"

#include <limits>
#include <omp.h>

using std::numeric_limits;
using std::max;
using glm::ivec2;

class Rasteriser : public SdlScreen {
private:
  const shared_ptr<const vector<Triangle>> triangles;
  Camera camera;

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
