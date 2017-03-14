#pragma once

#include <iostream>
#include <map>
#include <string>
#include <utility>
#include <vector>

#include "lodepng.h"
#include "triangle.h"

using std::map;
using std::string;
using std::vector;

class Object {
private:
  map<string, Material> materials;
  map<string, vector<Triangle>> groups;

protected:
  template <typename... MatArgs>
  void AddMaterial(string Name, MatArgs &&... matArgs) {
    materials.emplace(Name, std::forward<MatArgs>(matArgs)...));
  }

  template <typename... FaceArgs>
  void AddFace(string groupName, FaceArgs &&... faceArgs, string matName) {
    groups[groupName].emplace_back(std::forward<MatArgs>(faceArgs),
                                   materials[matName]);
  }

public:
  Object() {}

  virtual ~Object() {}

  const vector<Triangle> &operator[](string name) const { return groups[name]; }
};
