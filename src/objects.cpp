#include "objects.h"

BoundingVolume Box::createBoundingVolume() {
  BoundingVolume room(groups["room"]);

  room.setSubVolume(BoundingVolume(groups["short"]));
  room.setSubVolume(BoundingVolume(groups["tall"]));

  return room;
}

BoundingVolume Teapot::createBoundingVolume() {
  return BoundingVolume(groups[""]);
}
