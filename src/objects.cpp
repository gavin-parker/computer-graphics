#include "objects.h"

Box::Box() { load("obj-converter/box.sobj"); }

BoundingVolume Box::createBoundingVolume() {
  BoundingVolume room = getBoundingVolume("room");

  room.setSubVolume(getBoundingVolume("short"));
  room.setSubVolume(getBoundingVolume("tall"));

  return room;
}

Teapot::Teapot() { load("obj-converter/teapot.sobj"); }

BoundingVolume Teapot::createBoundingVolume() { return getBoundingVolume(""); }
