#include "raytracer.h"

RayTracer::RayTracer(int width, int height,  bool fullscreen):
	SdlScreen(width, height, fullscreen),
	camera(vec3(277.5f, 277.5f, -480.64), static_cast<float>(M_PI), 30.0f),
	light(vec3(400.0f, 100.0f, 100.0f), vec3(1.0, 1.0f, 1.0f), 500000.0f) {
	triangles = loadTestModel();
}

void RayTracer::update(float dt) {
	light.update(dt);
	camera.update(dt);
}

void RayTracer::draw(int width, int height) {
	#pragma omp parallel for
	for (int y = 0; y < height; ++y) {
		for (int x = 0; x < width; ++x) {
			Ray cameraRay, lightRay;

			camera.calculateRay(cameraRay, static_cast<float>(x) / width, static_cast<float>(y) / height);

			if (ClosestIntersection(cameraRay, triangles)) {
				light.calculateRay(lightRay, cameraRay.collisionLocation);
				ClosestIntersection(lightRay, triangles);

				vec3 lightColour = ambientLight;

				if (lightRay.collision == cameraRay.collision) {
					lightColour += light.directLight(cameraRay);
				}

				vec3 rayColour = cameraRay.collisionColor;
				drawPixel(x, y, vec3(
							  std::min(rayColour.r * lightColour.r, 1.0f),
							  std::min(rayColour.g * lightColour.g, 1.0f),
							  std::min(rayColour.b * lightColour.b, 1.0f)
							  ));
            }
        }
    }
}

bool RayTracer::ClosestIntersection(Ray &ray, const vector<Triangle> &triangles)  {
	ray.length = numeric_limits<float>::max();

	bool anyIntersection = false;

	for (const Triangle &triangle: triangles) {
		anyIntersection |= triangle.calculateIntection(ray);
    }

	return anyIntersection;
}


