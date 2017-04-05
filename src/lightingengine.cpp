#include "lightingengine.h"

LightingEngine::LightingEngine(const Ptr_Triangles &triangles,
                               const Light &light)
    : triangles(triangles), light(light){};
