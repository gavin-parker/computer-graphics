#include "rasteriser.h"

Rasteriser::Rasteriser(int width, int height, bool fullscreen)
    : SdlScreen(width, height, fullscreen),
      camera(vec3(277.5f, 277.5f, -480.64), static_cast<float>(M_PI), 30.0f) {
  triangles = loadTestModel();
}

void Rasteriser::update(float dt) { camera.update(dt); }

void Rasteriser::draw(int width, int height) {

  for (Triangle triangle : triangles) {

    vector<vec3> vertices(3);
    vertices[0] = triangle.v0;
    vertices[1] = triangle.v0 + triangle.e1;
    vertices[2] = triangle.v0 + triangle.e2;
    drawPolygonEdges(vertices);
  }
}

void Rasteriser::drawPolygonEdges(const vector<vec3> &vertices) {
  vector<vec2> proj(3);
  for (size_t i = 0; i < 3; i++) {
    camera.VertexShader(vertices[i], proj[i]);
  }
  vec3 color(1, 1, 1);
  drawEdge(proj[0], proj[1], color);
  drawEdge(proj[1], proj[2], color);
  drawEdge(proj[2], proj[0], color);
}

void Rasteriser::drawEdge(vec2 a, vec2 b, vec3 color) {
  float step = 1 / glm::distance(a, b);
  for (float t = 0; t < 1; t += step) {
    vec2 pixel = lerp(a, b, t);
    drawPixel(pixel.x, pixel.y, color);
  }
}
