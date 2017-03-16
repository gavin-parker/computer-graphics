#include "wavefront.h"

WaveFrontRenderer::WaveFrontRenderer(int width, int height, shared_ptr<LightingEngine> lighting,
	shared_ptr<Light> light,
	const shared_ptr<const vector<Triangle>> triangles, const shared_ptr<BoundingVolume> boundingVolume,
	bool fullscreen, bool antialias)
	: SdlScreen(width, height, fullscreen), triangles(triangles),
	camera(vec3(277.5f, 277.5f, -480.64), 0.0f, 30.0f), light(light),
	lighting(lighting), boundingVolume(boundingVolume), rayCaster(new RayCaster(triangles, boundingVolume, true)), antialias(antialias), rayIndices(vector<int>(width*height * 4 * 4)) {}

void WaveFrontRenderer::update(float dt) {
	light->update(dt);
	camera.update(dt);
}

void WaveFrontRenderer::draw(int width, int height) {
	//fire off initial rays
	int margin_y = height;
	int margin_x = width;
	margin_y--;
	margin_x--;
	rayCaster->flushBuffer();
//#pragma omp parallel for
	for (int y = 0; y < margin_y; ++y) {
		for (int x = 0; x < margin_x; ++x) {
			int pixelIndex = y*width + x;
			vec3 average(0, 0, 0);
			for (int i = 0; i < 4; i++) {
				for (int j = 0; j < 4; j++) {
					float ii, jj;
					ii = 0.25 * (float)i;
					jj = 0.25 * (float)j;

					float super_x = static_cast<float>(x) + ii;
					float super_y = static_cast<float>(y) + jj;
					Ray cameraRay;

					camera.calculateRay(cameraRay, super_x / width,
						super_y / height);

					cameraRay.length = numeric_limits<float>::max();
					#pragma omp critical
					{
						rayIndices[pixelIndex*16 + i * 4 + j] = rayCaster->enqueueRay(cameraRay);
					}
				}

			}

		}
	}
	cout << "firing initial rays \n";
	rayCaster->castRays(true);
	cout << "rays collided \n";
//#pragma omp parallel for
	for (int y = 0; y < margin_y; ++y) {
		for (int x = 0; x < margin_x; ++x) {
			int pixelIndex = y*width + x;
			vec3 average(0, 0, 0);
			for (int i = 0; i < 4; i++) {
				for (int j = 0; j < 4; j++) {
					bool anyCollision = false;
					Ray cameraRay = rayCaster->getRay(rayIndices[pixelIndex*16 + i * 4 + j], anyCollision);
					if (anyCollision) {
						//shared_ptr<const Material> mat = cameraRay.collision->mat;
						vec3 spec(0, 0, 0);
						vec3 lightColour = cameraRay.collision->colour;//lighting->calculateLight(cameraRay, glm::ivec2(x, y));
						average += lightColour;
					}
					else {
						average = vec3(0, 1, 0);
					}
				}
			}
			average /= 16.f;
			average = glm::normalize(average);
			drawPixel(x, y, vec3(std::min(average.r, 1.0f),
				std::min(average.g, 1.0f),
				std::min(average.b, 1.0f)));
		}
	}

	//get results from initial rays

	//compute GI
}
