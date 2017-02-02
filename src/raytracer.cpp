#include "raytracer.h"

RayTracer::RayTracer(int width, int height,  bool fullscreen):
	SdlScreen(width, height, fullscreen),
	camera(vec3(277.5f, 277.5f, -480.64), static_cast<float>(M_PI), 30.0f),
	light(vec3(400.0f, 100.0f, 100.0f), vec3(1.0, 1.0f, 1.0f), 1000000.0f) {
	triangles = loadTestModel();
}

void RayTracer::update(float dt) {
	light.update(dt);
	camera.update(dt);
}

void RayTracer::draw(int width, int height) {
	for (int y = 0; y < height; ++y) {
		for (int x = 0; x < width; ++x) {
			Ray ray;

			camera.calculateRay(ray, static_cast<float>(x) / width, static_cast<float>(y) / height);

			if (ClosestIntersection(ray, triangles)) {
				vec3 triangleColour = ray.collision->color;
				vec3 lightColour = light.directLight(ray.collisionLocation, ray.collision->normal);
				drawPixel(x, y, vec3(
							  std::min(triangleColour.r * lightColour.r, 1.0f),
							  std::min(triangleColour.g * lightColour.g, 1.0f),
							  std::min(triangleColour.b * lightColour.b, 1.0f)
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


