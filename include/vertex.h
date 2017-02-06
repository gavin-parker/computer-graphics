#pragma once
#include <glm/glm.hpp>
using glm::vec3;
using glm::vec2;

class Vertex {
public:
  vec3 position;
  vec3 normal;
  vec2 reflectance;
  vec3 illumination;
  Vertex()
      : position(vec3(0, 0, 0)), normal(vec3(0, 0, 0)), reflectance(vec2(0, 0)),
        illumination(vec3(0, 0, 0)) {}
  Vertex(vec3 position)
      : position(position), normal(vec3(0, 0, 0)), reflectance(vec2(0, 0)),
        illumination(vec3(0, 0, 0)) {}
  Vertex(vec3 position, vec3 normal)
      : position(position), normal(normal), reflectance(vec2(1, 1)),
        illumination(vec3(0, 0, 0)) {}
  Vertex(vec3 position, vec3 normal, vec2 reflectance, vec3 illumination)
      : position(position), normal(normal), reflectance(reflectance),
        illumination(illumination) {}
};
