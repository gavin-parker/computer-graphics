#include "raytracer.h"

RayTracer::RayTracer(int width, int height, shared_ptr<LightingEngine> lighting,
                     shared_ptr<PointLight> light,
                     const shared_ptr<const vector<Triangle>> triangles, const shared_ptr<BoundingVolume> boundingVolume,
                     bool fullscreen)
    : SdlScreen(width, height, fullscreen), triangles(triangles),
      camera(vec3(277.5f, 277.5f, -480.64), 0.0f, 30.0f), light(light),
      lighting(lighting), boundingVolume(boundingVolume) {}

void RayTracer::update(float dt) {
  light->update(dt);
  camera.update(dt);
}

void RayTracer::draw(int width, int height) {
	static int rows_completed = 0;
	static int counter_last = 0;
#pragma omp parallel for
  for (int y = 0; y < height; ++y) {
    for (int x = 0; x < width; ++x) {

      Ray cameraRay;

      camera.calculateRay(cameraRay, static_cast<float>(x) / width,
                          static_cast<float>(y) / height);
	  cameraRay.length = numeric_limits<float>::max();
      if (boundingVolume->calculateIntersection(cameraRay)) {
        shared_ptr<const Material> mat = cameraRay.collision->mat;

        vec3 spec(0, 0, 0);

		vec3 lightColour = lighting->calculateLight(cameraRay, glm::ivec2(x,y));

        drawPixel(x, y, vec3(std::min(lightColour.r, 1.0f),
                             std::min(lightColour.g, 1.0f),
                             std::min(lightColour.b, 1.0f)));
      }
    }
#pragma omp critical
	{
	rows_completed++;
	int percent_done =  static_cast<float>(rows_completed) /static_cast<float>(height) * 100.f;
	if (percent_done % 10 == 0 && percent_done > counter_last) {
		cout << percent_done << "%\n";
		counter_last = percent_done;
	}
	}
  }
  lighting->countedSamples++;
  rows_completed = 0;
  counter_last = 0;
}
