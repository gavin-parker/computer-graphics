#pragma once

#include <algorithm>
#include <glm/glm.hpp>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "lodepng.h"
#include "scale.h"

using glm::vec2;
using glm::vec4;
using std::cout;
using std::endl;
using std::min;
using std::shared_ptr;
using std::string;
using std::vector;

class Texture {
private:
  static const unsigned PNG_PIXEL_SIZE = 4;

  vec4 scale;
  bool hasTexture;
  unsigned width, height;
  vector<unsigned char> texture;

public:
  Texture();

  Texture(vec4 scale, const string &textureFile);

  vec4 getScale() const;

  vec4 operator[](vec2 uv) const;
};
