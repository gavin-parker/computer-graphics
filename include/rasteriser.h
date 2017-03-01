#pragma once

#include "camera.h"
#include "lightingengine.h"
#include "rastlighting.h"
#include "baked_gi.h"
#include "light.h"
#include "sdlscreen.h"
#include "testmodel.h"
#include <limits>
#include <omp.h>

using std::numeric_limits;
using std::max;
using glm::ivec2;
using glm::vec2;

class Rasteriser : public SdlScreen {
private:
  vector<float> depthBuffer;

  const shared_ptr<const vector<Triangle>> triangles;
  Camera camera;
  shared_ptr<Light> light;
  shared_ptr<LightingEngine> lighting;

  void computePolygonRows(const vector<Pixel> &vertexPixels,
                          vector<Pixel> &leftPixels,
                          vector<Pixel> &rightPixels, const Triangle &triangle);
  void drawPolygonRows(int width, int height, vector<Pixel> &leftPixels,
                       vector<Pixel> &rightPixels, const Triangle &triangle);

  Pixel VertexShader(Vertex v, int width, int height);
protected:
  void update(float dt) override;
  void draw(int width, int height) override;

public:
  Rasteriser(int width, int height, shared_ptr<LightingEngine> lighting, shared_ptr<Scene> scene, bool fullscreen = false);
};
