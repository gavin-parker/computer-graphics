#include "objects.h"

Cube::Cube(){
AddMaterial(string("cube"), vec3(1.0, 1.0, 1.0), vec3(1.0, 1.0, 1.0), vec3(0.0, 0.0, 0.0), 0, string(""), string(""), string(""), string(""));
AddGroup(string("a"));
AddFace(string("a"), vec3(0.0, 0.0, 0.0), vec3(1.0, 1.0, 0.0), vec3(1.0, 0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(0.0, 0.0, -1.0), vec3(0.0, 0.0, -1.0), vec3(0.0, 0.0, -1.0), string("[cube]"));
AddFace(string("a"), vec3(0.0, 0.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(1.0, 1.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(0.0, 0.0, -1.0), vec3(0.0, 0.0, -1.0), vec3(0.0, 0.0, -1.0), string("[cube]"));
AddGroup(string("b"));
AddFace(string("b"), vec3(0.0, 0.0, 0.0), vec3(0.0, 1.0, 1.0), vec3(0.0, 1.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(-1.0, 0.0, 0.0), vec3(-1.0, 0.0, 0.0), vec3(-1.0, 0.0, 0.0), string("[cube]"));
AddFace(string("b"), vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 1.0), vec3(0.0, 1.0, 1.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(-1.0, 0.0, 0.0), vec3(-1.0, 0.0, 0.0), vec3(-1.0, 0.0, 0.0), string("[cube]"));
AddGroup(string("c"));
AddFace(string("c"), vec3(0.0, 1.0, 0.0), vec3(1.0, 1.0, 1.0), vec3(1.0, 1.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0), string("[cube]"));
AddFace(string("c"), vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 1.0), vec3(1.0, 1.0, 1.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0), string("[cube]"));
AddGroup(string("cube"));
AddFace(string("cube"), vec3(0.0, 0.0, 0.0), vec3(1.0, 1.0, 0.0), vec3(1.0, 0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(0.0, 0.0, -1.0), vec3(0.0, 0.0, -1.0), vec3(0.0, 0.0, -1.0), string("[cube]"));
AddFace(string("cube"), vec3(0.0, 0.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(1.0, 1.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(0.0, 0.0, -1.0), vec3(0.0, 0.0, -1.0), vec3(0.0, 0.0, -1.0), string("[cube]"));
AddFace(string("cube"), vec3(0.0, 0.0, 0.0), vec3(0.0, 1.0, 1.0), vec3(0.0, 1.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(-1.0, 0.0, 0.0), vec3(-1.0, 0.0, 0.0), vec3(-1.0, 0.0, 0.0), string("[cube]"));
AddFace(string("cube"), vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 1.0), vec3(0.0, 1.0, 1.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(-1.0, 0.0, 0.0), vec3(-1.0, 0.0, 0.0), vec3(-1.0, 0.0, 0.0), string("[cube]"));
AddFace(string("cube"), vec3(0.0, 1.0, 0.0), vec3(1.0, 1.0, 1.0), vec3(1.0, 1.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0), string("[cube]"));
AddFace(string("cube"), vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 1.0), vec3(1.0, 1.0, 1.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(0.0, 1.0, 0.0), string("[cube]"));
AddFace(string("cube"), vec3(1.0, 0.0, 0.0), vec3(1.0, 1.0, 0.0), vec3(1.0, 1.0, 1.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0), string("[cube]"));
AddFace(string("cube"), vec3(1.0, 0.0, 0.0), vec3(1.0, 1.0, 1.0), vec3(1.0, 0.0, 1.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0), string("[cube]"));
AddFace(string("cube"), vec3(0.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 1.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(0.0, -1.0, 0.0), vec3(0.0, -1.0, 0.0), vec3(0.0, -1.0, 0.0), string("[cube]"));
AddFace(string("cube"), vec3(0.0, 0.0, 0.0), vec3(1.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(0.0, -1.0, 0.0), vec3(0.0, -1.0, 0.0), vec3(0.0, -1.0, 0.0), string("[cube]"));
AddFace(string("cube"), vec3(0.0, 0.0, 1.0), vec3(1.0, 0.0, 1.0), vec3(1.0, 1.0, 1.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0), string("[cube]"));
AddFace(string("cube"), vec3(0.0, 0.0, 1.0), vec3(1.0, 1.0, 1.0), vec3(0.0, 1.0, 1.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0), string("[cube]"));
AddGroup(string("d"));
AddFace(string("d"), vec3(1.0, 0.0, 0.0), vec3(1.0, 1.0, 0.0), vec3(1.0, 1.0, 1.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0), string("[cube]"));
AddFace(string("d"), vec3(1.0, 0.0, 0.0), vec3(1.0, 1.0, 1.0), vec3(1.0, 0.0, 1.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0), string("[cube]"));
AddGroup(string("e"));
AddFace(string("e"), vec3(0.0, 0.0, 0.0), vec3(1.0, 0.0, 0.0), vec3(1.0, 0.0, 1.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(0.0, -1.0, 0.0), vec3(0.0, -1.0, 0.0), vec3(0.0, -1.0, 0.0), string("[cube]"));
AddFace(string("e"), vec3(0.0, 0.0, 0.0), vec3(1.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(0.0, -1.0, 0.0), vec3(0.0, -1.0, 0.0), vec3(0.0, -1.0, 0.0), string("[cube]"));
AddGroup(string("f"));
AddFace(string("f"), vec3(0.0, 0.0, 1.0), vec3(1.0, 0.0, 1.0), vec3(1.0, 1.0, 1.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0), string("[cube]"));
AddFace(string("f"), vec3(0.0, 0.0, 1.0), vec3(1.0, 1.0, 1.0), vec3(0.0, 1.0, 1.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec2(0.0, 0.0), vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, 1.0), string("[cube]"));
}
