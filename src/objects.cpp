#include "objects.h"

#include "../obj-converter/box.cpp"

BoundingVolume Box::createBoundingVolume() {
  BoundingVolume room(groups["room"]);

  room.setSubVolume(BoundingVolume(groups["short"]));
  room.setSubVolume(BoundingVolume(groups["tall"]));

  return room;
}
