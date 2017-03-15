#pragma once

#include <iostream>
#include <map>
#include <string>
#include <utility>
#include <vector>

#include "bvh.h"
#include "lodepng.h"
#include "triangle.h"

using std::map;
using std::string;
using std::vector;

class Object {
protected:
  map<string, Material> materials;
  map<string, vector<Triangle>> groups;

  template <typename... MatArgs>
  void AddMaterial(string Name, MatArgs &&... matArgs) {
    materials.emplace(Name, Material(std::forward<MatArgs>(matArgs)...));
  }

  void AddFace(string groupName, vec3 v0, vec3 v1, vec3 v2, vec2 vt0, vec2 vt1,
               vec2 vt2, string matName) {
    groups[groupName].emplace_back(v0, v1, v2, vt0, vt1, vt2,
                                   &materials[matName]);
  }

  void AddFace(string groupName, vec3 v0, vec3 v1, vec3 v2, vec2 vt0, vec2 vt1,
               vec2 vt2, vec3 vn0, vec3 vn1, vec3 vn2, string matName) {
    groups[groupName].emplace_back(v0, v1, v2, vt0, vt1, vt2, vn0, vn1, vn2,
                                   &materials[matName]);
  }

public:
  Object() {}

  virtual ~Object() {}

  vector<Triangle> allTriangles() {
    vector<Triangle> triangles;

    for (const auto &group : groups) {
      triangles.insert(triangles.end(), group.second.begin(),
                       group.second.end());
    }

    return triangles;
  }

  virtual BoundingVolume createBoundingVolume() = 0;
};
