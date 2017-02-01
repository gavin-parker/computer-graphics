#include "raytracer.h"

RayTracer::RayTracer(int width, int height,  bool fullscreen):
	SdlScreen(width, height, fullscreen),
	camera(vec3(277.5f, 277.5f, -480.64), static_cast<float>(M_PI), 30.0f) {
	triangles = loadTestModel();
}

void RayTracer::update(float dt) {
	camera.update(dt);
}

void RayTracer::draw(int width, int height) {
	for (int y = 0; y < height; ++y) {
		for (int x = 0; x < width; ++x) {
			Ray ray;
            Intersection closestIntersection;
			closestIntersection.triangle = nullptr;

			camera.calculateRay(ray, static_cast<float>(x) / width, static_cast<float>(y) / height);

			if (ClosestIntersection(ray, triangles, closestIntersection)) {
				drawPixel(x,y, closestIntersection.triangle->color);
            }
        }
    }
}

bool RayTracer::ClosestIntersection(Ray ray, const vector<Triangle> &triangles, Intersection &closestIntersection)  {
	closestIntersection.distance = numeric_limits<float>::max();

	bool anyIntersection = false;

	for(const Triangle &triangle: triangles) {

		if (glm::dot(triangle.normal, ray.direction) < 0) {

			vec3
					e1 = triangle.v1 - triangle.v0,
					e2 = triangle.v2 - triangle.v0,
					b = ray.position - triangle.v0;

			glm::mat3 A(-ray.direction, e1, e2);

			vec3 x = glm::inverse(A) * b;

			//bit dodge
			if(x[1] >= 0 && x[2] >= 0 && (x[1] + x[2]) < 1 && x[0] >= 0 && x[0] < closestIntersection.distance){
				closestIntersection.distance = x[0];
				closestIntersection.triangle = &triangle;
				closestIntersection.position = x;

				anyIntersection = true;
			}
		}
    }

	return anyIntersection;
}


