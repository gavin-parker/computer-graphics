#include "raytracer.h"

RayTracer::RayTracer(int width, int height,  bool fullscreen):
	SdlScreen(width, height, fullscreen),
	camera(vec3(0, 0, -1), height / 2.0f){
	triangles = LoadTestModel();
}

void RayTracer::update(float dt) {
	camera.Update(dt);
}

void RayTracer::draw(int width, int height) {
	for (int y=0; y < height; y++) {
		for (int x=0; x < width; x++) {
            Intersection closestIntersection;
			closestIntersection.triangle = nullptr;
			vec3 d(x - width / 2.0f, y - width / 2.0f, camera.focalLength);
			if (ClosestIntersection(camera.position, d, triangles, closestIntersection)) {
				drawPixel(x,y, closestIntersection.triangle->color);
            }
        }
    }
}

bool RayTracer::ClosestIntersection(vec3 start, vec3 dir, const vector<Triangle> &triangles, Intersection &closestIntersection)  {
	closestIntersection.distance = numeric_limits<float>::max();

	bool anyIntersection = false;

	for(const Triangle &triangle: triangles) {

		if (glm::dot(triangle.normal, dir) < 0) {

			vec3
					e1 = triangle.v1 - triangle.v0,
					e2 = triangle.v2 - triangle.v0,
					b = start - triangle.v0;

			glm::mat3 A(-dir, e1, e2);

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


