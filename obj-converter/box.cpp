#include "objects.h"

Box::Box() {
  AddMaterial(string("green"), vec3(1.0, 1.0, 1.0), vec3(1.0, 1.0, 1.0),
              vec3(0.0, 0.0, 0.0), 0, string(""), string(""), string(""),
              string(""));
  AddMaterial(string("red"), vec3(1.0, 1.0, 1.0), vec3(1.0, 1.0, 1.0),
              vec3(0.0, 0.0, 0.0), 0, string(""), string(""), string(""),
              string(""));
  AddMaterial(string("short"), vec3(1.0, 1.0, 1.0), vec3(1.0, 1.0, 1.0),
              vec3(0.0, 0.0, 0.0), 0, string(""), string(""), string(""),
              string(""));
  AddMaterial(string("tall"), vec3(1.0, 1.0, 1.0), vec3(1.0, 1.0, 1.0),
              vec3(0.0, 0.0, 0.0), 0, string(""), string(""), string(""),
              string(""));
  AddMaterial(string("white"), vec3(1.0, 1.0, 1.0), vec3(1.0, 1.0, 1.0),
              vec3(0.0, 0.0, 0.0), 0, string(""), string(""), string(""),
              string(""));
  AddFace(string("room"), vec3(555.0, 0.0, 555.0), vec3(0.0, 0.0, 0.0),
          vec3(555.0, 0.0, 0.0), vec2(0, 0), vec2(1, 1), vec2(0, 1),
          string("white"));
  AddFace(string("room"), vec3(555.0, 0.0, 555.0), vec3(0.0, 0.0, 555.0),
          vec3(0.0, 0.0, 0.0), vec2(0, 0), vec2(1, 0), vec2(1, 1),
          string("white"));
  AddFace(string("room"), vec3(555.0, 0.0, 0.0), vec3(555.0, 555.0, 0.0),
          vec3(555.0, 0.0, 555.0), vec2(0, 1), vec2(0, 0), vec2(1, 1),
          string("green"));
  AddFace(string("room"), vec3(555.0, 0.0, 555.0), vec3(555.0, 555.0, 0.0),
          vec3(555.0, 555.0, 555.0), vec2(1, 1), vec2(0, 0), vec2(1, 0),
          string("green"));
  AddFace(string("room"), vec3(0.0, 555.0, 0.0), vec3(0.0, 0.0, 0.0),
          vec3(0.0, 0.0, 555.0), vec2(1, 0), vec2(1, 1), vec2(0, 1),
          string("red"));
  AddFace(string("room"), vec3(0.0, 555.0, 555.0), vec3(0.0, 555.0, 0.0),
          vec3(0.0, 0.0, 555.0), vec2(0, 0), vec2(1, 0), vec2(0, 1),
          string("red"));
  AddFace(string("room"), vec3(555.0, 555.0, 0.0), vec3(0.0, 555.0, 0.0),
          vec3(555.0, 555.0, 555.0), vec2(0, 0), vec2(1, 0), vec2(0, 1),
          string("white"));
  AddFace(string("room"), vec3(0.0, 555.0, 0.0), vec3(0.0, 555.0, 555.0),
          vec3(555.0, 555.0, 555.0), vec2(1, 0), vec2(1, 1), vec2(0, 1),
          string("white"));
  AddFace(string("room"), vec3(555.0, 555.0, 555.0), vec3(0.0, 0.0, 555.0),
          vec3(555.0, 0.0, 555.0), vec2(0, 0), vec2(1, 1), vec2(0, 1),
          string("white"));
  AddFace(string("room"), vec3(555.0, 555.0, 555.0), vec3(0.0, 555.0, 555.0),
          vec3(0.0, 0.0, 555.0), vec2(0, 0), vec2(1, 0), vec2(1, 1),
          string("white"));
  AddFace(string("short"), vec3(290.0, 165.0, 114.0), vec3(130.0, 0.0, 65.0),
          vec3(290.0, 0.0, 114.0), vec2(0, 0), vec2(1, 1), vec2(0, 1),
          string("short"));
  AddFace(string("short"), vec3(290.0, 165.0, 114.0), vec3(130.0, 165.0, 65.0),
          vec3(130.0, 0.0, 65.0), vec2(0, 0), vec2(1, 0), vec2(1, 1),
          string("short"));
  AddFace(string("short"), vec3(130.0, 165.0, 65.0), vec3(82.0, 0.0, 225.0),
          vec3(130.0, 0.0, 65.0), vec2(0, 0), vec2(1, 1), vec2(0, 1),
          string("short"));
  AddFace(string("short"), vec3(130.0, 165.0, 65.0), vec3(82.0, 165.0, 225.0),
          vec3(82.0, 0.0, 225.0), vec2(0, 0), vec2(1, 0), vec2(1, 1),
          string("short"));
  AddFace(string("short"), vec3(82.0, 165.0, 225.0), vec3(240.0, 0.0, 272.0),
          vec3(82.0, 0.0, 225.0), vec2(0, 0), vec2(1, 1), vec2(0, 1),
          string("short"));
  AddFace(string("short"), vec3(82.0, 165.0, 225.0), vec3(240.0, 165.0, 272.0),
          vec3(240.0, 0.0, 272.0), vec2(0, 0), vec2(1, 0), vec2(1, 1),
          string("short"));
  AddFace(string("short"), vec3(240.0, 165.0, 272.0), vec3(290.0, 165.0, 114.0),
          vec3(240.0, 0.0, 272.0), vec2(0, 0), vec2(1, 0), vec2(0, 1),
          string("short"));
  AddFace(string("short"), vec3(290.0, 165.0, 114.0), vec3(290.0, 0.0, 114.0),
          vec3(240.0, 0.0, 272.0), vec2(1, 0), vec2(1, 1), vec2(0, 1),
          string("short"));
  AddFace(string("short"), vec3(240.0, 165.0, 272.0), vec3(130.0, 165.0, 65.0),
          vec3(290.0, 165.0, 114.0), vec2(0, 0), vec2(1, 1), vec2(0, 1),
          string("short"));
  AddFace(string("short"), vec3(240.0, 165.0, 272.0), vec3(82.0, 165.0, 225.0),
          vec3(130.0, 165.0, 65.0), vec2(0, 0), vec2(1, 0), vec2(1, 1),
          string("short"));
  AddFace(string("tall"), vec3(423.0, 330.0, 247.0), vec3(265.0, 0.0, 296.0),
          vec3(423.0, 0.0, 247.0), vec2(0, 0), vec2(1, 1), vec2(0, 1),
          string("tall"));
  AddFace(string("tall"), vec3(423.0, 330.0, 247.0), vec3(265.0, 330.0, 296.0),
          vec3(265.0, 0.0, 296.0), vec2(0, 0), vec2(1, 0), vec2(1, 1),
          string("tall"));
  AddFace(string("tall"), vec3(265.0, 330.0, 296.0), vec3(314.0, 0.0, 456.0),
          vec3(265.0, 0.0, 296.0), vec2(0, 0), vec2(1, 1), vec2(0, 1),
          string("tall"));
  AddFace(string("tall"), vec3(265.0, 330.0, 296.0), vec3(314.0, 330.0, 456.0),
          vec3(314.0, 0.0, 456.0), vec2(0, 0), vec2(1, 0), vec2(1, 1),
          string("tall"));
  AddFace(string("tall"), vec3(314.0, 330.0, 456.0), vec3(423.0, 0.0, 247.0),
          vec3(314.0, 0.0, 456.0), vec2(0, 0), vec2(1, 1), vec2(0, 1),
          string("tall"));
  AddFace(string("tall"), vec3(314.0, 330.0, 456.0), vec3(472.0, 330.0, 406.0),
          vec3(472.0, 0.0, 406.0), vec2(0, 0), vec2(1, 0), vec2(1, 1),
          string("tall"));
  AddFace(string("tall"), vec3(472.0, 330.0, 406.0), vec3(423.0, 330.0, 247.0),
          vec3(472.0, 0.0, 406.0), vec2(0, 0), vec2(1, 0), vec2(0, 1),
          string("tall"));
  AddFace(string("tall"), vec3(423.0, 330.0, 247.0), vec3(423.0, 0.0, 247.0),
          vec3(472.0, 0.0, 406.0), vec2(1, 0), vec2(1, 1), vec2(0, 1),
          string("tall"));
  AddFace(string("tall"), vec3(472.0, 330.0, 406.0), vec3(265.0, 330.0, 296.0),
          vec3(423.0, 330.0, 247.0), vec2(0, 0), vec2(1, 1), vec2(0, 1),
          string("tall"));
  AddFace(string("tall"), vec3(472.0, 330.0, 406.0), vec3(314.0, 330.0, 456.0),
          vec3(265.0, 330.0, 296.0), vec2(0, 0), vec2(1, 0), vec2(1, 1),
          string("tall"));
}
