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
  vector<float> depthBuffer;

  const shared_ptr<const vector<Triangle>> triangles;
  Camera camera;
  PointLight light;

  void computePolygonRows(const vector<Pixel> &vertexPixels,
                          vector<Pixel> &leftPixels,
                          vector<Pixel> &rightPixels);
  void drawPolygonRows(int width, int height, vector<Pixel> &leftPixels,
                       vector<Pixel> &rightPixels);

  Pixel VertexShader(Vertex v, int width, int height);

protected:
  void update(float dt) override;
  void draw(int width, int height) override;

public:
  Rasteriser(int width, int height, bool fullscreen = false);
};
