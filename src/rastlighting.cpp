#include "rastlighting.h"

// GlobalIllumination::GlobalIllumination(){};

RastLighting::RastLighting(const shared_ptr<Scene> scene,
                           int lightMapResolution)
    : LightingEngine(scene->triangles, scene->light),
      boundingVolume(scene->volume), lightMapResolution(lightMapResolution),
      depthMap(vector<float>(lightMapResolution * lightMapResolution)){};

vec3 RastLighting::calculateLight(Ray &ray, ivec2 pixel) {
  vec3 lightColour(0, 0, 0);
  Ray lightRay(light->position, ray.collisionLocation - light->position);
  const Material &mat = *(ray.getCollision()->mat);
  vec3 n = ray.collision->normal;
  vec3 v = glm::normalize(ray.direction);
  vec3 l = glm::normalize(lightRay.direction);
  vec3 spec = mat->phong(v, l, n);
  lightColour += light->directLight(ray) * mat.diffuse + spec * mat.specularity;

  return lightColour;
}

bool RastLighting::ClosestIntersection(Ray &ray) {
  ray.length = numeric_limits<float>::max();

  bool anyIntersection = false;

  for (const Triangle &triangle : *triangles) {
    anyIntersection |= triangle.calculateIntersection(ray);
  }

  return anyIntersection;
}

// like closestIntersection, but backs out after a single intersection
bool RastLighting::anyIntersection(Ray &ray, Ray &surface) {
  bool anyIntersection = false;
  float lightDistance = ray.length;
  ray.length = numeric_limits<float>::max();
  for (const Triangle &triangle : *triangles) {
    anyIntersection |= triangle.calculateIntersection(ray);
    if (anyIntersection && ray.collision != surface.collision &&
        ray.length < lightDistance) {
      return anyIntersection;
    }
  }
  return anyIntersection;
}

void RastLighting::fillShadowMap() {
  for (const Triangle &triangle : *triangles) {
    vector<Vertex> vertices = {
        Vertex(triangle.v0, triangle.normal, vec2(1, 1), triangle.colour),
        Vertex(triangle.v1, triangle.normal, vec2(1, 1), triangle.colour),
        Vertex(triangle.v2, triangle.normal, vec2(1, 1), triangle.colour)};
    vector<Pixel> proj(vertices.size());
    for (size_t i = 0; i < vertices.size(); i++) {
      // proj[i] = light->projectVertex(vertices[i].position, );
    }
  }
}
