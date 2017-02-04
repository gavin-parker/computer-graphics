#include "textureloader.h"

TextureLoader::TextureLoader(){};

bool TextureLoader::LoadTexture(std::string filename, vector<unsigned char> &texture){
    std::cout << "loading texture: " << filename;
    unsigned width, height;

    unsigned error = lodepng::decode(texture, width, height, filename);

    if(error){
        std::cout << "Error loading texture: " << error << ":" << lodepng_error_text(error);
        return false;
    }
    return true;
}