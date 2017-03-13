#pragma once

#include "camera.h"
#include "lightingengine.h"
#include "rastlighting.h"
#include "baked_gi.h"
#include "flatlighting.h"
#include "sdlscreen.h"
#include "testmodel.h"
#include <limits>
#include <omp.h>

#define INSIDE 0
#define LEFT 1
#define RIGHT 2
#define BOTTOM 4
#define TOP 8 


using std::numeric_limits;
using std::max;
using glm::ivec2;
using glm::vec2;
using glm::vec4;
using glm::ivec4;


class Rasteriser : public SdlScreen {
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
  bool useShadows;
  void computePolygonRows(const vector<Pixel> &vertexPixels,
                          vector<Pixel> &leftPixels,
                          vector<Pixel> &rightPixels, const Triangle &triangle);
  void drawPolygonRows(int width, int height, vector<Pixel> &leftPixels,
                       vector<Pixel> &rightPixels, const Triangle &triangle);

  Pixel VertexShader(Vertex v, int width, int height);

  void shadowPass(int width, int height,
	  vector<Pixel> &leftPixels,
	  vector<Pixel> &rightPixels, const Triangle &triangle);
  void clip(int width, int height);
protected:
  void update(float dt) override;
  void draw(int width, int height) override;

public:
  Rasteriser(int width, int height, shared_ptr<LightingEngine> lighting, shared_ptr<Scene> scene,vec3 cameraPos,bool useShadows = true, bool fullscreen = false);
};
