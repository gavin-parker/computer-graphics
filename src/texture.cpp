#include "texture.h"

Texture::Texture() : Texture(vec4(), "") {}

Texture::Texture(vec4 scale, const string &textureFile)
    : scale(scale), hasTexture(false), width(0), height(0) {

  if (textureFile != "") {
    cout << "loading texture: " << textureFile << endl;
    unsigned error = lodepng::decode(texture, width, height, textureFile);

    if (error) {
      cout << "Error loading texture: " << error << ":"
           << lodepng_error_text(error) << endl;
    } else {
      hasTexture = true;
    }
  }
}

vec4 Texture::operator[](vec2 uv) const {
  if (hasTexture) {
    unsigned x = std::floor(uv.x * width);
    unsigned y = std::floor(uv.y * height);

    size_t index = min((width * y + x) * PNG_PIXEL_SIZE,
                       static_cast<unsigned>(texture.size() - PNG_PIXEL_SIZE));

    return (1.0f / 255.0f) *
           scaleVec(scale,
                    vec4(static_cast<vec4::value_type>(texture[index + 0]),
                         static_cast<vec4::value_type>(texture[index + 1]),
                         static_cast<vec4::value_type>(texture[index + 2]),
                         static_cast<vec4::value_type>(texture[index + 3])));
  } else {
    return scale;
  }
}
