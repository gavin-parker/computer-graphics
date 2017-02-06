#include "rasteriser.h"

Rasteriser::Rasteriser(int width, int height, bool fullscreen)
    : SdlScreen(width, height, fullscreen), depthBuffer(width * height),
      triangles(loadTestModel()),
      camera(vec3(277.5f, 277.5f, -480.64), 0.0f, 30.0f) {}

void Rasteriser::update(float dt) { camera.update(dt); }

void Rasteriser::draw(int width, int height) {
  for (size_t i = 0; i < depthBuffer.size(); ++i) {
    depthBuffer[i] = numeric_limits<float>::max();
  }

  for (const Triangle &triangle : *triangles) {
    vector<vec3> vertices = {triangle.v0, triangle.v1, triangle.v2};
    vector<Pixel> proj(vertices.size());
    for (size_t i = 0; i < vertices.size(); i++) {
      vec3 camSpace = camera.VertexShader(vertices[i]);
      proj[i] =
          Pixel(static_cast<int>(width * (1 - camSpace.x) / 2.0),
                static_cast<int>(height * (1 - camSpace.y) / 2.0), camSpace.z);
    }

    int projE1X = proj[1].x - proj[0].x;
    int projE1Y = proj[1].y - proj[0].y;
    int projE2X = proj[2].x - proj[0].x;
    int projE2Y = proj[2].y - proj[0].y;

    if (projE1X * projE2Y > projE2X * projE1Y) {
      vector<Pixel> leftPixels;
      vector<Pixel> rightPixels;
      computePolygonRows(proj, leftPixels, rightPixels);
      drawPolygonRows(width, height, leftPixels, rightPixels, triangle.colour);
    }
  }
}

void Rasteriser::drawPolygonRows(int width, int height,
                                 vector<Pixel> &leftPixels,
                                 vector<Pixel> &rightPixels, vec3 color) {
  for (size_t y = 0; y < leftPixels.size(); y++) {
    for (int x = leftPixels[y].x; x <= rightPixels[y].x; x++) {
      float pixelDepth = lerpF(leftPixels[y].depth, rightPixels[y].depth,
                               deLerpF(leftPixels[y].x, rightPixels[y].x, x));

      if (pixelDepth > 0 && x >= 0 && x < static_cast<int>(width) &&
          leftPixels[y].y >= 0 && leftPixels[y].y < static_cast<int>(height)) {
        float &bufferDepth = depthBuffer[width * leftPixels[y].y + x];

        if (pixelDepth < bufferDepth) {
          bufferDepth = pixelDepth;
          drawPixel(x, leftPixels[y].y, color);
        }
      }
    }
  }
}

void Rasteriser::computePolygonRows(const vector<Pixel> &vertexPixels,
                                    vector<Pixel> &leftPixels,
                                    vector<Pixel> &rightPixels) {
  int max = -numeric_limits<int>::max();
  int min = numeric_limits<int>::max();

  for (size_t i = 0; i < vertexPixels.size(); i++) {
    max = std::max(vertexPixels[i].y, max);
    min = std::min(vertexPixels[i].y, min);
  }
  int rows = max - min + 1;
  for (int i = 0; i < rows; i++) {
    leftPixels.push_back(Pixel(numeric_limits<int>::max(), 0, 0.0f));
    rightPixels.push_back(Pixel(-numeric_limits<int>::max(), 0, 0.0f));
  }
  for (int i = 0; i < 3; i++) {
    const Pixel &start = vertexPixels[i];
    const Pixel &end = vertexPixels[(i + 1) % 3];
    float step =
        1.f / (glm::length(vec2(start.x - end.x, start.y - end.y)) + 1);
    for (float t = 0; t < 1; t += step) {
      Pixel pixel = lerpP(vertexPixels[i], vertexPixels[(i + 1) % 3], t);
      int y = pixel.y - min;
      if (pixel.x < leftPixels[y].x) {
        leftPixels[y] = pixel;
      }
      if (pixel.x > rightPixels[y].x) {
        rightPixels[y] = pixel;
      }
    }
  }
}
