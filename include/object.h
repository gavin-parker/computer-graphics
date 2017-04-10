#pragma once

#include <cstdio>
#include <iostream>
#include <map>
#include <string>
#include <utility>
#include <vector>

#include "bvh.h"
#include "lodepng.h"
#include "triangle.h"

using std::cerr;
using std::endl;
using std::map;
using std::string;
using std::vector;

class Object {
private:
  map<string, const Material *> materials;
  map<string, Ptr_Triangles> groups;

  void readMaterials(FILE *file);

  void readMaterial(FILE *file);

  void readGroups(FILE *file);

  void readGroup(FILE *file);

  void readFace(FILE *file);

  void skipText(FILE *file, const string &text);

  bool readBool(FILE *file);

  unsigned readCount(FILE *file);

  vec4::value_type readScalar(FILE *file);

  string readString(FILE *file);

  vec2 readVec2(FILE *file);

  vec3 readVec3(FILE *file);

protected:
  BoundingVolume getBoundingVolume(string groupName);

  void load(string fileName);

public:
  Object();

  virtual ~Object();

  Ptr_Triangles allTriangles();

  virtual BoundingVolume createBoundingVolume() = 0;
};
