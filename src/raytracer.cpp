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
            Intersection closestIntersection;
			closestIntersection.triangle = nullptr;

			camera.calculateRay(ray, static_cast<float>(x) / width, static_cast<float>(y) / height);

			if (ClosestIntersection(ray, triangles, closestIntersection)) {
				vec3 triangleColour = closestIntersection.triangle->color;
				vec3 lightColour = light.directLight(closestIntersection.position, closestIntersection.triangle->normal);
				drawPixel(x, y, vec3(
							  std::min(triangleColour.r * lightColour.r, 1.0f),
							  std::min(triangleColour.g * lightColour.g, 1.0f),
							  std::min(triangleColour.b * lightColour.b, 1.0f)
							  ));
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
					closestIntersection.position = triangle.v0 + u * e1 + v * e2;

					anyIntersection = true;
				}
			}
		}
    }

	return anyIntersection;
}


