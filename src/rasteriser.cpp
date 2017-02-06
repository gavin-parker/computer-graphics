#include "rasteriser.h"

Rasteriser::Rasteriser(int width, int height, bool fullscreen)
    : SdlScreen(width, height, fullscreen), triangles(loadTestModel()),
      camera(vec3(277.5f, 277.5f, -480.64), static_cast<float>(M_PI), 30.0f) {}

void Rasteriser::update(float dt) { camera.update(dt); }

void Rasteriser::draw(int width, int height) {
  for (const Triangle &triangle : *triangles) {

    vector<vec3> vertices(3);
    vertices[0] = triangle.v0;
    vertices[1] = triangle.v0 + triangle.e1;
    vertices[2] = triangle.v0 + triangle.e2;
    vector<ivec2> proj(3);
    for (size_t i = 0; i < 3; i++) {
      camera.VertexShader(vertices[i], proj[i]);
    }
    vector<ivec2> leftPixels;
    vector<ivec2> rightPixels;
    computePolygonRows(proj, leftPixels, rightPixels);
    drawPolygonRows(leftPixels, rightPixels, triangle.colour);
  }
}

void Rasteriser::drawEdge(vec2 a, vec2 b, vec3 color) {
  float step = 1 / glm::distance(a, b);
  for (float t = 0; t < 1; t += step) {
    vec2 pixel = lerp(a, b, t);
    drawPixel(pixel.x, pixel.y, color);
  }
}

void Rasteriser::drawPolygonRows(vector<ivec2> &leftPixels,
                                 vector<ivec2> &rightPixels, vec3 color) {
  for (size_t y = 0; y < leftPixels.size(); y++) {
    for (int x = leftPixels[y].x; x < rightPixels[y].x; x++) {
      drawPixel(x, leftPixels[y].y, color);
    }
  }
}

void Rasteriser::computePolygonRows(const vector<ivec2> &vertexPixels,
                                    vector<ivec2> &leftPixels,
                                    vector<ivec2> &rightPixels) {
  int max = -numeric_limits<int>::max();
  int min = numeric_limits<int>::max();

  for (size_t i = 0; i < vertexPixels.size(); i++) {
    max = std::max(vertexPixels[i].y, max);
    min = std::min(vertexPixels[i].y, min);
  }
  int rows = max - min + 1;
  for (int i = 0; i < rows; i++) {
    leftPixels.push_back(ivec2(numeric_limits<int>::max(), min + i));
    rightPixels.push_back(ivec2(-numeric_limits<int>::max(), min + i));
  }
  for (int i = 0; i < 3; i++) {
    float step =
        1 / glm::length(vec2(vertexPixels[i] - vertexPixels[(i + 1) % 3]));
    for (float t = 0; t < 1; t += step) {
      ivec2 pixel = lerp(vertexPixels[i], vertexPixels[(i + 1) % 3], t);
      int y = pixel.y - min;
      leftPixels[y].x = std::min(leftPixels[y].x, pixel.x);
      rightPixels[y].x = std::max(rightPixels[y].x, pixel.x);
    }
  }
}
