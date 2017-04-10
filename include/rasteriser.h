#pragma once

#include <limits>
#include <omp.h>

#include "camera.h"
#include "flatlighting.h"
#include "lightingengine.h"
#include "rastlighting.h"
#include "sdlscreen.h"
#include <limits>
#include <omp.h>
#include <unordered_set>

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
  const Ptr_Triangles &triangles;
  Ptr_Triangles clipped_triangles;
  Camera camera;
  Light &light;
  LightingEngine &lighting;
  vector<vector<Pixel>> leftBuffer;
  vector<vector<Pixel>> rightBuffer;

  int computeClipping(float x, float y, int xMax, int yMax);

  vec4 CohenSutherland(vec2 A, vec2 B, ivec2 bounds);

  void clip(int width, int height);

  Pixel VertexShader(Vertex v, int width, int height);

  void shadowPass(int width, int height, vector<Pixel> &leftPixels,
                  vector<Pixel> &rightPixels, const Triangle &triangle);

  bool useShadows;
  void computePolygonRows(const vector<Pixel> &vertexPixels,
                          vector<Pixel> &leftPixels, vector<Pixel> &rightPixels,
                          const Triangle &triangle);
  void drawPolygonRows(int width, int height, vector<Pixel> &leftPixels,
                       vector<Pixel> &rightPixels, const Triangle &triangle);

protected:
  void update(float dt) override;
  void draw(int width, int height) override;

public:
  Rasteriser(int width, int height, LightingEngine &lighting, Scene &scene,
             vec3 cameraPos = vec3(277.5f, 277.5f, -480.64),
             bool useShadows = true, bool fullscreen = false);
};

namespace std {
template <> struct hash<vec3> {
  size_t operator()(const vec3 &k) const {
    return std::hash<float>()(k.x) ^ std::hash<float>()(k.y) ^
           std::hash<float>()(k.z);
  }

  bool operator()(const vec3 &a, const vec3 &b) const {
    return a.x == b.x && a.y == b.y && a.z == b.z;
  }
};
}
