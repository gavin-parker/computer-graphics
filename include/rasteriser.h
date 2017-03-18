#pragma once

#include <limits>
#include <omp.h>

#include "baked_gi.h"
#include "camera.h"
#include "lightingengine.h"
#include "rastlighting.h"
#include "sdlscreen.h"
#include "testmodel.h"

using std::numeric_limits;
using std::max;
using glm::ivec2;
using glm::vec2;
using glm::vec4;
using glm::ivec4;

class Rasteriser : public SdlScreen {
public:
  enum Clipping : int { INSIDE = 0, LEFT = 1, RIGHT = 2, BOTTOM = 4, TOP = 8 };

private:
  vector<float> depthBuffer;
  vector<float> shadowBuffer;
  const shared_ptr<const vector<Triangle>> triangles;
  vector<Triangle> clipped_triangles;
  Camera camera;
  shared_ptr<Light> light;
  shared_ptr<LightingEngine> lighting;
  vector<vector<Pixel>> leftBuffer;
  vector<vector<Pixel>> rightBuffer;

  int computeClipping(float x, float y, int xMax, int yMax);

  vec4 CohenSutherland(vec2 A, vec2 B, ivec2 bounds);

  void clip(int width, int height);

  Pixel VertexShader(Vertex v, int width, int height);

  void shadowPass(int width, int height, vector<Pixel> &leftPixels,
                  vector<Pixel> &rightPixels, const Triangle &triangle);

  void computePolygonRows(const vector<Pixel> &vertexPixels,
                          vector<Pixel> &leftPixels, vector<Pixel> &rightPixels,
                          const Triangle &triangle);
  void drawPolygonRows(int width, int height, vector<Pixel> &leftPixels,
                       vector<Pixel> &rightPixels, const Triangle &triangle);

protected:
  void update(float dt) override;
  void draw(int width, int height) override;

public:
  Rasteriser(int width, int height, shared_ptr<LightingEngine> lighting,
             shared_ptr<Scene> scene, bool fullscreen = false);
};
