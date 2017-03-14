#include "raytracer.h"

RayTracer::RayTracer(int width, int height, shared_ptr<LightingEngine> lighting,
                     shared_ptr<Light> light,
                     const shared_ptr<const vector<Triangle>> triangles, const shared_ptr<BoundingVolume> boundingVolume,
                     bool fullscreen, bool antialias)
    : SdlScreen(width, height, fullscreen), triangles(triangles),
      camera(vec3(277.5f, 277.5f, -480.64), 0.0f, 30.0f), light(light),
      lighting(lighting), boundingVolume(boundingVolume), antialias(antialias) {}

void RayTracer::update(float dt) {
  light->update(dt);
  camera.update(dt);
}

void RayTracer::draw(int width, int height) {
	static int rows_completed = 0;
	static int counter_last = 0;

	int margin_y = height;
	int margin_x = width;
	if (antialias) {
		margin_y--;
		margin_x--;
	}

#pragma omp parallel for
  for (int y = 0; y < margin_y; ++y) {
    for (int x = 0; x < margin_x; ++x) {
		vec3 average(0,0,0);
		float step = 0.25f;
		for (float i = 0; i < 1; i+=step) {
			for (float j = 0; j < 1; j+=step) {
				float super_x = static_cast<float>(x) + i;
				float super_y = static_cast<float>(y) + j;
				Ray cameraRay;

				camera.calculateRay(cameraRay, super_x / width,
					super_y / height);

				cameraRay.length = numeric_limits<float>::max();
				if (boundingVolume->calculateIntersection(cameraRay, true)) {
					shared_ptr<const Material> mat = cameraRay.collision->mat;

					vec3 spec(0, 0, 0);

					vec3 lightColour = lighting->calculateLight(cameraRay, glm::ivec2(x, y));

					average += lightColour;

				}
			}
			average /= 16.f;
			drawPixel(x, y, vec3(std::min(average.r, 1.0f),
				std::min(average.g, 1.0f),
				std::min(average.b, 1.0f)));
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
