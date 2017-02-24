#include "rasteriser.h"

Rasteriser::Rasteriser(int width, int height, shared_ptr<LightingEngine> lighting, shared_ptr<Scene> scene, bool fullscreen)
    : SdlScreen(width, height, fullscreen), depthBuffer(width * height),
      triangles(scene->triangles),
      camera(vec3(277.5f, 277.5f, -480.64), 0.0f, 30.0f), light(scene->light), lighting(lighting) {}

void Rasteriser::update(float dt) {
  camera.update(dt);
  light->update(dt);
}

void Rasteriser::draw(int width, int height) {
  for (size_t i = 0; i < depthBuffer.size(); ++i) {
    depthBuffer[i] = numeric_limits<float>::max();
  }
  for (const Triangle &triangle : *triangles) {
    vector<Vertex> vertices = {
        Vertex(triangle.v0, triangle.normal, vec2(1, 1), triangle.colour),
        Vertex(triangle.v1, triangle.normal, vec2(1, 1), triangle.colour),
        Vertex(triangle.v2, triangle.normal, vec2(1, 1), triangle.colour)};
    vector<Pixel> proj(vertices.size());

	//here is where we do our vertex shading
    for (size_t i = 0; i < vertices.size(); i++) {
		//Ray ray;
		//ray.collisionLocation = vertices[i].position;
		//ray.collision = &triangle;
		//ray.direction = camera.position - vertices[i].position;
		//vec3 illumination = lighting->calculateLight(ray);
		proj[i] = VertexShader(vertices[i], width, height);
		//proj[i].v.illumination = illumination;

    }

    int projE1X = proj[1].x - proj[0].x;
    int projE1Y = proj[1].y - proj[0].y;
    int projE2X = proj[2].x - proj[0].x;
    int projE2Y = proj[2].y - proj[0].y;

    if (projE1X * projE2Y > projE2X * projE1Y) {
      vector<Pixel> leftPixels;
      vector<Pixel> rightPixels;
      computePolygonRows(proj, leftPixels, rightPixels, triangle);
      drawPolygonRows(width, height, leftPixels, rightPixels, triangle);
    }
  }
}

void Rasteriser::drawPolygonRows(int width, int height,
                                 vector<Pixel> &leftPixels,
                                 vector<Pixel> &rightPixels, const Triangle &triangle) {
#pragma omp parallel for
  for (int y = 0; y < static_cast<int>(leftPixels.size()); y++) {

	  //
	  //ray.direction = camera.position - rightPixels[y].v.position;
	  //ray.collisionLocation = rightPixels[y].v.position;
	  //rightPixels[y].v.illumination = lighting->calculateLight(ray);


    for (int x = leftPixels[y].x; x <= rightPixels[y].x; x++) {
      float pixelDepth = lerpF(leftPixels[y].depth, rightPixels[y].depth,
                               deLerpF(leftPixels[y].x, rightPixels[y].x, x));

      if (pixelDepth > 0 && x >= 0 && x < static_cast<int>(width) &&
          leftPixels[y].y >= 0 && leftPixels[y].y < static_cast<int>(height)) {
        float &bufferDepth = depthBuffer[width * leftPixels[y].y + x];

        if (pixelDepth < bufferDepth) {
          bufferDepth = pixelDepth;
          Vertex pixelVert =
              lerpV(leftPixels[y].v, rightPixels[y].v,leftPixels[y].depth, rightPixels[y].depth, pixelDepth,
                    deLerpF(leftPixels[y].x, rightPixels[y].x, x));

		  vec3 f1 = triangle.v0 - pixelVert.position;
		  vec3 f2 = triangle.v1 - pixelVert.position;
		  vec3 f3 = triangle.v2 - pixelVert.position;
		  float a = glm::length(glm::cross(triangle.e1, triangle.e2));
		  float a1 = glm::length(glm::cross(f2, f3)) / a;
		  float a2 = glm::length(glm::cross(f3, f1)) / a;
		  float a3 = glm::length(glm::cross(f1, f2)) / a;
		  vec2 uv(a2, a3);
		  //if calculating per pixel...
		  Ray ray;
		  vec3 realPos = pixelVert.position;
		  ray.direction = camera.position - realPos;
		  ray.collisionLocation = realPos;
		  ray.collision = &triangle;
		  pixelVert.illumination = lighting->calculateLight(ray);
		  vec3 lightColour = triangle.colour*pixelVert.illumination;
		  drawPixel(x, leftPixels[y].y, vec3(std::min(lightColour.r, 1.0f),
			  std::min(lightColour.g, 1.0f),
			  std::min(lightColour.b, 1.0f)));
        }
      }
    }
  }
}

void Rasteriser::computePolygonRows(const vector<Pixel> &vertexPixels,
                                    vector<Pixel> &leftPixels,
                                    vector<Pixel> &rightPixels, const Triangle &triangle) {
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

Pixel Rasteriser::VertexShader(Vertex v, int width, int height) {
  vec3 camSpace = camera.projectVertex(v);
  return Pixel(static_cast<int>(width * (1 - camSpace.x) / 2.0),
               static_cast<int>(height * (1 - camSpace.y) / 2.0), camSpace.z,
               v);
}
