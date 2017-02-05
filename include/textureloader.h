#include <iostream>
#include <string>
#include <vector>

#include "lodepng.h"

using std::vector;

class TextureLoader {
private:
public:
  TextureLoader();

  // currently I will only support 128x128 png images
  bool LoadTexture(std::string filename, vector<unsigned char> &texture);
};
