#include "raytracer.h"

RayTracer::RayTracer(int width, int height,  bool fullscreen):
	SdlScreen(width, height, fullscreen){
    LoadTestModel(triangles);
    camera = vec3(0,0,0);
}

void RayTracer::update(float dt) {

}

void RayTracer::draw() {
    float focal_length = getHeight()/2;
    for(int y=0; y < getHeight(); y++){
        for(int x=0; x < getWidth(); x++){
            Intersection closestIntersection;
            vec3 d(x - getWidth()/2, y-getWidth()/2, focal_length);
            if(ClosestIntersection(camera, d, triangles, closestIntersection)){
                drawPixel(x,y, triangles[closestIntersection.triangleIndex].color);
            }
        }
    }
}

bool RayTracer::ClosestIntersection(vec3 start, vec3 dir, const vector<Triangle> &triangles, Intersection& closestIntersection){
    closestIntersection.distance = numeric_limits<float>::max();
    for(size_t i = 0; i < triangles.size(); i++){
        Triangle triangle = triangles[i];
        vec3 e1 = triangle.v1 - triangle.v0;
        vec3 e2 = triangle.v2 - triangle.v0;
        vec3 b = start - triangle.v0;
        glm::mat3 A(-dir, e1, e2);
        vec3 x = glm::inverse(A)*b;
        //bit dodge
        if(x[1] > 0 && x[2] > 0 && (x[1] + x[2]) < 1 && x[0] >= 0 && x[0] < closestIntersection.distance){
            closestIntersection.distance = x[0];
            closestIntersection.triangleIndex = i;
            closestIntersection.position = x;
        }
    }
    if(closestIntersection.distance == numeric_limits<float>::max()){
        return false;
    }
return true;
}


