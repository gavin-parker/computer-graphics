#include "raytracer.h"

RayTracer::RayTracer(int width, int height,  bool fullscreen):
	SdlScreen(width, height, fullscreen),
	camera(vec3(0, 0, -1), height / 2.0f){
	triangles = LoadTestModel();
}

void RayTracer::update(float dt) {
	camera.Update(dt);
}

void RayTracer::draw() {
    for(int y=0; y < getHeight(); y++){
        for(int x=0; x < getWidth(); x++){
            Intersection closestIntersection;
            closestIntersection.triangleIndex = 0;
			vec3 d(x - getWidth()/2, y-getWidth()/2, camera.focalLength);
			if(ClosestIntersection(camera.position, d, triangles, closestIntersection)) {
				drawPixel(x,y, triangles[closestIntersection.triangleIndex].color);
            }
        }
    }
}

bool RayTracer::ClosestIntersection(vec3 start, vec3 dir, const vector<Triangle> &triangles, Intersection &closestIntersection){
    closestIntersection.distance = numeric_limits<float>::max();
    for(size_t i = 0; i < triangles.size(); i++){
		const Triangle &triangle = triangles[i];

		if (glm::dot(triangle.normal, dir) < 0) {

			vec3 e1 = triangle.v1 - triangle.v0;
			vec3 e2 = triangle.v2 - triangle.v0;
			vec3 b = start - triangle.v0;
			glm::mat3 A(-dir, e1, e2);
			vec3 x = glm::inverse(A)*b;
			//bit dodge
			if(x[1] >= 0 && x[2] >= 0 && (x[1] + x[2]) < 1 && x[0] >= 0 && x[0] < closestIntersection.distance){
				closestIntersection.distance = x[0];
				closestIntersection.triangleIndex = i;
				closestIntersection.position = x;
			}
		}
    }
    if(closestIntersection.distance == numeric_limits<float>::max()){
        return false;
    }
return true;
}


