#include "rastlighting.h"

RastLighting::RastLighting(const Scene &scene, int lightMapResolution)
    : LightingEngine(scene.triangles, scene.light),
      boundingVolume(scene.volume), lightMapResolution(lightMapResolution),
      depthMap(vector<float>(lightMapResolution * lightMapResolution)){};

vec3 RastLighting::calculateLight(Ray &ray, ivec2 pixel) {
  vec3 lightColour(0, 0, 0);

  lightColour +=
      light.directLight(ray) * ray.collisionDiffuseColour() +
      ray.collisionSpecularColour(ray.collisionLocation() - light.position);
  return lightColour;
}

bool RastLighting::ClosestIntersection(Ray &ray) {
  ray.extendToInfinity();

  bool anyIntersection = false;

  for (const Ptr_Triangle &triangle : triangles) {
    anyIntersection |= triangle->calculateIntersection(ray);
  }

  return anyIntersection;
}

// like closestIntersection, but backs out after a single intersection
bool RastLighting::anyIntersection(Ray &ray, Ray &surface) {
  bool anyIntersection = false;
  float lightDistance = ray.getLength();
  ray.extendToInfinity();
  for (const Ptr_Triangle &triangle : triangles) {
    anyIntersection |= triangle->calculateIntersection(ray);
    if (anyIntersection && ray.getCollision() != surface.getCollision() &&
        ray.getLength() < lightDistance) {
      return anyIntersection;
    }
  }
  return anyIntersection;
}

void RastLighting::fillShadowMap() {
  /*for (const Triangle &triangle : triangles) {
    vector<Vertex> vertices = {
        Vertex(triangle.v0, triangle.normal, vec2(1, 1), triangle.colour),
        Vertex(triangle.v1, triangle.normal, vec2(1, 1), triangle.colour),
        Vertex(triangle.v2, triangle.normal, vec2(1, 1), triangle.colour)};
    vector<Pixel> proj(vertices.size());
    for (size_t i = 0; i < vertices.size(); i++) {
      // proj[i] = light.projectVertex(vertices[i].position, );
    }
  }*/
}
