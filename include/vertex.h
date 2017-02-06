#pragma once

class Vertex {
public:
  vec3 position;
  vec3 normal;
  vec2 reflectance;

  Vertex()
      : position(vec3(0, 0, 0)), normal(vec3(0, 0, 0)),
        reflectance(vec2(0, 0)) {}
  Vertex(vec3 position)
      : position(position), normal(vec3(0, 0, 0)), reflectance(vec2(0, 0)) {}
  Vertex(vec3 position, vec3 normal)
      : position(position), normal(normal), reflectance(vec2(1, 1)) {}
  Vertex(vec3 position, vec3 normal, vec2 reflectance)
      : position(position), normal(normal), reflectance(reflectance) {}
};
