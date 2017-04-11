#include "object.h"

void Object::readMaterials(FILE *file) {
  skipText(file, "Materials: ");

  unsigned materialCount = readCount(file);

  for (unsigned n = 0; n < materialCount; ++n) {
    readMaterial(file);
  }
}

void Object::readMaterial(FILE *file) {
  skipText(file, "Material: ");
  string materialName = readString(file);
  vec3 ka = readVec3(file);
  vec3 kd = readVec3(file);
  vec3 ks = readVec3(file);
  vec4::value_type ns = readScalar(file);
  string mapKa = readString(file);
  string mapKd = readString(file);
  string mapKs = readString(file);
  string mapNs = readString(file);
  bool isMirror = readBool(file);
  bool isRefractive = readBool(file);

  materials.emplace(materialName,
                    new Material(ka, kd, ks, ns, mapKa, mapKd, mapKs, mapNs,
                                 isMirror, isRefractive));
}

void Object::readGroups(FILE *file) {
  skipText(file, "Groups: ");

  unsigned groupCount = readCount(file);

  for (unsigned n = 0; n < groupCount; ++n) {
    readGroup(file);
  }
}

void Object::readGroup(FILE *file) {
  skipText(file, "Group: ");

  string groupName = readString(file);

  groups.emplace(groupName, Ptr_Triangles());

  unsigned faceCount = readCount(file);

  for (unsigned n = 0; n < faceCount; ++n) {
    readFace(file);
  }
}

void Object::readFace(FILE *file) {
  skipText(file, "Face: ");

  string groupName = readString(file);

  vec3 v0 = readVec3(file);
  vec3 v1 = readVec3(file);
  vec3 v2 = readVec3(file);

  vec2 vt0 = readVec2(file);
  vec2 vt1 = readVec2(file);
  vec2 vt2 = readVec2(file);

  vec3 vn0, vn1, vn2;

  bool calculateNormals = readBool(file);

  if (calculateNormals) {
    skipText(file, "null null null ");
  } else {
    vn0 = readVec3(file);
    vn1 = readVec3(file);
    vn2 = readVec3(file);
  }

  string materialName = readString(file);

  if (calculateNormals) {
    groups[groupName].push_back(
        new Triangle(v0, v1, v2, vt0, vt1, vt2, materials[materialName]));
  } else {
    groups[groupName].push_back(new Triangle(
        v0, v1, v2, vt0, vt1, vt2, vn0, vn1, vn2, materials[materialName]));
  }
}

void Object::skipText(FILE *file, const string &text) {
  string format = text + "%n";
  int charsRead;
  int itemsRead = fscanf(file, format.data(), &charsRead);

  if (charsRead == 0 || itemsRead != 0) {
    cerr << "Could not skip \"" << text << "\"" << endl;
  }
}

bool Object::readBool(FILE *file) {
  int value;

  if (fscanf(file, "%i ", &value) != 1) {
    cerr << "Failed to read bool" << endl;
  }

  return value == 1;
}

unsigned Object::readCount(FILE *file) {
  unsigned value;

  if (fscanf(file, "%u ", &value) != 1) {
    cerr << "Failed to read count" << endl;
  }

  return value;
}

vec4::value_type Object::readScalar(FILE *file) {
  vec4::value_type value;

  if (fscanf(file, "%f ", &value) != 1) {
    cerr << "Failed to read scalar" << endl;
  }

  return value;
}

string Object::readString(FILE *file) {
  size_t stringLength;

  if (fscanf(file, "s(%zd,", &stringLength) != 1) {
    cerr << "Failed to read string header" << endl;
  }

  string value(stringLength, 0);

  if (fread((void *)value.data(), sizeof(string::value_type), stringLength,
            file) != stringLength) {
    cerr << "Failed to read string";
  }

  skipText(file, ") ");

  return value;
}

vec2 Object::readVec2(FILE *file) {
  float x, y;

  if (fscanf(file, "%f %f ", &x, &y) != 2) {
    cerr << "Failed to read vec2" << endl;
  }

  return vec2(x, y);
}

vec3 Object::readVec3(FILE *file) {
  float x, y, z;

  if (fscanf(file, "%f %f %f ", &x, &y, &z) != 3) {
    cerr << "Failed to read vec2" << endl;
  }

  return vec3(x, y, z);
}

BoundingVolume Object::getBoundingVolume(string groupName) {
  return BoundingVolume(groups[groupName]);
}

void Object::load(string fileName) {
  FILE *file = fopen(fileName.data(), "r");

  readMaterials(file);
  readGroups(file);

  fclose(file);
}

Object::Object(BRDF &brdf) { materials.emplace("", new Material(brdf)); }

Object::Object() { materials.emplace("", new Material()); }

Object::~Object() {
  for (const auto &group : groups) {
    for (const auto triangle : group.second) {
      delete triangle;
    }
  }

  for (auto material : materials) {
    delete material.second;
  }
}

Ptr_Triangles Object::allTriangles() {
  Ptr_Triangles triangles;

  for (const auto &group : groups) {
    triangles.insert(triangles.end(), group.second.begin(), group.second.end());
  }

  return triangles;
}
