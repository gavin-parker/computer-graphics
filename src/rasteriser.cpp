#include "rasteriser.h"

Rasteriser::Rasteriser(int width, int height, float viewAngle,
                       LightingEngine &lighting, Scene &scene, bool useShadows,
                       bool fullscreen)
    : ObjectScreen(width, height, viewAngle, lighting, scene, fullscreen),
      triangles(scene.triangles), clipped_triangles(Ptr_Triangles()),
      leftBuffer(triangles.size()), rightBuffer(triangles.size()) {}

int Rasteriser::computeClipping(float x, float y, int xMax, int yMax) {
  int clipping = INSIDE;

  if (x < -xMax) {
    clipping |= LEFT;
  } else if (x > xMax) {
    clipping |= RIGHT;
  }
  if (y < -yMax) {
    clipping |= BOTTOM;
  } else if (y > yMax) {
    clipping |= TOP;
  }
  return clipping;
}
vec4 Rasteriser::CohenSutherland(vec2 A, vec2 B, ivec2 bounds) {
  int clipA = computeClipping(A.x, A.y, bounds.x, bounds.y);
  int clipB = computeClipping(B.x, B.y, bounds.x, bounds.y);
  while (true) {
    if (!(clipA | clipB)) {
      // line completely within screen
      break;
    } else if (clipA & clipB) {
      // line completely out of screen
      break;
    } else {
      // line intersects border
      float x = 0.0f, y = 0.0f;
      int clippedPoint = clipA ? clipA : clipB;

      if (clippedPoint & TOP) {
        x = A.x + (B.x - A.x) * (bounds.y - A.y) / (B.y - A.y);
        y = bounds.y;
      } else if (clippedPoint & BOTTOM) {
        x = A.x + (B.x - A.x) * ((-bounds.y) - A.y) / (B.y - A.y);
        y = (-bounds.y);
      } else if (clippedPoint & RIGHT) {
        y = A.y + (B.y - A.y) * (bounds.x - A.x) / (B.x - A.x);
        x = bounds.x;
      } else if (clippedPoint & LEFT) {
        y = A.y + (B.y - A.y) * ((-bounds.x) - A.x) / (B.x - A.x);
        x = (-bounds.x);
      }
      if (clippedPoint == clipA) {
        A.x = x;
        A.y = y;
        clipA = computeClipping(A.x, A.y, bounds.x, bounds.y);
      } else {
        B.x = x;
        B.y = y;
        clipB = computeClipping(B.x, B.y, bounds.x, bounds.y);
      }
    }
  }
  return vec4(A, B);
}

void Rasteriser::clip(int width, int height) {
  for (const Ptr_Triangle &triangle : triangles) {
    vector<vec3> vertices = triangle->getVertices();

    float xMax = (float)width / 2.0;
    float yMax = (float)height / 2.0;

    // vec4 lines[3];
    for (int i = 0; i < 3; i++) {
      vec4 homA = camera.clipSpace(vertices[i]);
      vec4 homB = camera.clipSpace(vertices[(i + 1) % 3]);
      vec2 A(homA.x, homA.y);
      vec2 B(homB.x, homB.y);
      vec4 line = CohenSutherland(A, B, ivec2(xMax, yMax));
      // if this vertex not changed
      if (line[0] == A.x && line[1] == A.y) {
      }
    }
    clipped_triangles.push_back(triangle);
  }
}

Pixel Rasteriser::VertexShader(vec3 v, int width, int height) {
  vec3 camSpace = camera.projectVertex(v);
  return Pixel(static_cast<int>(width * (1 - camSpace.x) / 2.0),
               static_cast<int>(height * (1 - camSpace.y) / 2.0), camSpace.z,
               v);
}

void Rasteriser::computePolygonRows(int width, int height,
                                    const vector<Pixel> &vertexPixels,
                                    vector<Pixel> &leftPixels,
                                    vector<Pixel> &rightPixels,
                                    const Triangle &triangle) {
  int max = numeric_limits<int>::min();
  int min = numeric_limits<int>::max();
  for (size_t i = 0; i < vertexPixels.size(); i++) {
    max = std::max(vertexPixels[i].y, max);
    min = std::min(vertexPixels[i].y, min);
  }
  int rows = max - min + 1;

  leftPixels.resize(rows);
  rightPixels.resize(rows);

  for (int i = 0; i < rows; i++) {
    leftPixels[i].x = numeric_limits<int>::max();
    rightPixels[i].x = numeric_limits<int>::min();
  }

  for (size_t i = 0; i < vertexPixels.size(); i++) {
    const Pixel &start = vertexPixels[i];
    const Pixel &end = vertexPixels[(i + 1) % vertexPixels.size()];
    float step =
        1.f / (glm::length(vec2(start.x - end.x, start.y - end.y)) + 1);
    for (float t = 0; t < 1; t += step) {
      Pixel pixel = lerpP(start, end, t);

      if (pixel.x < 0) {
        pixel.x = 0;
      } else if (pixel.x > width) {
        pixel.x = width;
      }
      if (pixel.y < 0) {
        pixel.y = 0;
      } else if (pixel.y > height) {
        pixel.y = height;
      }

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

ptrdiff_t Rasteriser::shadowBufferIndex(indexedPixel pixel) {
  return ((pixel.i * SHADOW_RESOLUTION) + pixel.y) * SHADOW_RESOLUTION +
         pixel.x;
}

void Rasteriser::shadowPass(int width, int height, vector<Pixel> &leftPixels,
                            vector<Pixel> &rightPixels,
                            const Triangle &triangle) {
  if (leftPixels.size() < 1) {
    return;
  }

  Pixel *l_pixels = leftPixels.data();
  Pixel *r_pixels = rightPixels.data();
  int rowCount = static_cast<int>(leftPixels.size());
  // gets the depth in 6 directions from the light source
  for (int y = 0; y < rowCount; y++) {
    for (int x = l_pixels[y].x; x <= r_pixels[y].x; x++) {
      vec3 pixelPosition = lerpV(l_pixels[y].position, r_pixels[y].position,
                                 deLerpI(l_pixels[y].x, r_pixels[y].x, x));

      if (glm::dot(triangle.normal, light.position - pixelPosition) > 0.0f) {
        float depth = 0.0f;
        indexedPixel lightPixel = light.projectVertex(pixelPosition, depth);
        if (lightPixel.i >= 0) {
          // shadowBuffer stores closest depths to light source
          if (depth < shadowBuffer[shadowBufferIndex(lightPixel)]) {
            shadowBuffer[shadowBufferIndex(lightPixel)] = depth;
          }
        }
      }
    }
  }
}

void Rasteriser::drawPolygonRows(int width, int height,
                                 vector<Pixel> &leftPixels,
                                 vector<Pixel> &rightPixels,
                                 const Triangle &triangle) {
  if (leftPixels.size() < 1) {
    return;
  }
  Pixel *l_pixels = leftPixels.data();
  Pixel *r_pixels = rightPixels.data();
  int rowCount = static_cast<int>(leftPixels.size());
#pragma omp parallel for
  for (int y = 0; y < rowCount; y++) {
    for (int x = l_pixels[y].x; x <= r_pixels[y].x; x++) {
      Pixel pixel = lerpP(l_pixels[y], r_pixels[y],
                          deLerpI(l_pixels[y].x, r_pixels[y].x, x));

      if (pixel.depth > 0 && x >= 0 && x < static_cast<int>(width) &&
          l_pixels[y].y >= 0 && l_pixels[y].y < static_cast<int>(height)) {
        float &bufferDepth = depthBuffer[width * l_pixels[y].y + x];

        if (pixel.depth < bufferDepth) {
          bufferDepth = pixel.depth;

          vec3 f1 = triangle.v0 - pixel.position;
          vec3 f2 = triangle.v1 - pixel.position;
          vec3 f3 = triangle.v2 - pixel.position;
          float a = glm::length(glm::cross(triangle.e1, triangle.e2));
          float a1 = glm::length(glm::cross(f2, f3)) / a;
          float a2 = glm::length(glm::cross(f3, f1)) / a;
          float a3 = glm::length(glm::cross(f1, f2)) / a;
          vec3 bary(a1, a2, a3);

          vec3 offset = pixel.position - camera.position;
          float distance = glm::length(offset);

          Ray cameraRay(camera.position, offset);

          cameraRay.updateCollision(&triangle, distance, bary);

          float depth = 0.f;
          indexedPixel lightPixel = light.projectVertex(pixel.position, depth);

          vec3 lightColour =
              lighting.ambientLight * cameraRay.collisionAmbientColour();

          if (lightPixel.i >= 0) {
            float d = shadowBuffer[shadowBufferIndex(lightPixel)];
            if (depth < (d + 10.f)) {
              lightColour +=
                  lighting.calculateLight(cameraRay, glm::ivec2(x, y));
            }
          }

          drawPixel(x, l_pixels[y].y, vec3(std::min(lightColour.r, 1.0f),
                                           std::min(lightColour.g, 1.0f),
                                           std::min(lightColour.b, 1.0f)));
        }
      }
    }
  }
}

void Rasteriser::draw(int width, int height) {
  clipped_triangles.clear();
  clip(width, height);

  depthBuffer.clear();
  depthBuffer.resize(width * height, numeric_limits<float>::max());

  shadowBuffer.clear();
  shadowBuffer.resize(SHADOW_DIRECTION_COUNT * SHADOW_RESOLUTION *
                          SHADOW_RESOLUTION,
                      numeric_limits<float>::max());

  for (size_t t = 0; t < clipped_triangles.size(); t++) {
    const Ptr_Triangle &triangle = clipped_triangles[t];

    vector<vec3> vertices = triangle->getVertices();

    vector<Pixel> proj(vertices.size());

    // here is where we do our vertex shading
    for (size_t i = 0; i < vertices.size(); i++) {
      proj[i] = VertexShader(vertices[i], width, height);
    }

    int projE1X = proj[1].x - proj[0].x;
    int projE1Y = proj[1].y - proj[0].y;
    int projE2X = proj[2].x - proj[0].x;
    int projE2Y = proj[2].y - proj[0].y;

    if (projE1X * projE2Y > projE2X * projE1Y) {
      computePolygonRows(width, height, proj, leftBuffer[t], rightBuffer[t],
                         *triangle);
      shadowPass(width, height, leftBuffer[t], rightBuffer[t], *triangle);
    }
  }
  for (size_t t = 0; t < clipped_triangles.size(); t++) {
    drawPolygonRows(width, height, leftBuffer[t], rightBuffer[t],
                    *clipped_triangles[t]);
  }
}
