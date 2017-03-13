#pragma once

#include <iostream>
#include <string>
#include <utility>
#include <vector>

#include "lodepng.h"

using std::string;

template <typename T> void dumpArgs(const T &t) { std::cout << t << std::endl; }

template <typename T, typename... Ts> void dumpArgs(const T &t, Ts &&... ts) {
  dumpArgs(t);
  dumpArgs(std::forward<Ts>(ts)...);
}

class Object {

protected:
  template <typename... MatArgs>
  void AddMaterial(string Name, MatArgs &&... matArgs) {
    dumpArgs(Name, std::forward<MatArgs>(matArgs)...);
  }

  template <typename... FaceArgs>
  void AddFace(string groupName, FaceArgs &&... faceArgs, string matName) {
    dumpArgs(groupName, std::forward<FaceArgs>(faceArgs)..., matName);
  }

public:
  Object() {}

  virtual ~Object() {}
};
