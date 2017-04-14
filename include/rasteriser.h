#pragma once

#include "objectscreen.h"

using glm::ivec2;
using glm::ivec4;

class Rasteriser : public ObjectScreen {
public:
  enum Clipping : int { INSIDE = 0, LEFT = 1, RIGHT = 2, BOTTOM = 4, TOP = 8 };

private:
  static const int SHADOW_DIRECTION_COUNT = 6;
  static const ptrdiff_t SHADOW_RESOLUTION = 128;

  vector<float> depthBuffer;
  vector<float> shadowBuffer;
  const Ptr_Triangles &triangles;
  Ptr_Triangles clipped_triangles;
  vector<vector<Pixel>> leftBuffer;
  vector<vector<Pixel>> rightBuffer;

  int computeClipping(float x, float y, int xMax, int yMax);

  vec4 CohenSutherland(vec2 A, vec2 B, ivec2 bounds);

  void clip(int width, int height);

  Pixel VertexShader(vec3 v, int width, int height);

  ptrdiff_t shadowBufferIndex(indexedPixel pixel);

  void shadowPass(int width, int height, vector<Pixel> &leftPixels,
                  vector<Pixel> &rightPixels, const Triangle &triangle);

  bool useShadows;
  void computePolygonRows(const vector<Pixel> &vertexPixels,
                          vector<Pixel> &leftPixels, vector<Pixel> &rightPixels,
                          const Triangle &triangle);
  void drawPolygonRows(int width, int height, vector<Pixel> &leftPixels,
                       vector<Pixel> &rightPixels, const Triangle &triangle);

protected:
  void draw(int width, int height) override;

public:
  Rasteriser(int width, int height, float viewAngle, LightingEngine &lighting,
             Scene &scene, bool useShadows = true, bool fullscreen = false);
};
