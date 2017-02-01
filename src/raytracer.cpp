#include "raytracer.h"

RayTracer::RayTracer(int width, int height,  bool fullscreen):
	SdlScreen(width, height, fullscreen),
	camera(vec3(277.5f, 277.5f, -480.64), static_cast<float>(M_PI), 30.0f),
	light(vec3(0.0f, 0.5f, -0.7f), 14.f*vec3(1, 1, 1)) {
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
				//vec3 n = Triangle::getNormal(closestIntersection.triangle->v0, closestIntersection.triangle->v1, closestIntersection.triangle->v2);
				//vec3 color = light.directLight(closestIntersection.position, n);
				//drawPixel(x, y, color);
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
			float det_A = glm::determinant(A);

			float t = glm::determinant(glm::mat3(b, e1, e2)) / det_A;
			if(t >= 0 && t < closestIntersection.distance){
				float u = glm::determinant(glm::mat3(-ray.direction, b, e2)) / det_A;
				float v = glm::determinant(glm::mat3(-ray.direction, e1, b)) / det_A;

				if(u >= 0 && v >= 0 && (u + v) < 1){
					closestIntersection.distance = t;
					closestIntersection.triangle = &triangle;
					closestIntersection.position = vec3(t, u, v);

					anyIntersection = true;
				}
			}
		}
    }

	return anyIntersection;
}


