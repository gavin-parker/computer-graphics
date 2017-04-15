#include "pointlight.h"

PointLight::PointLight(vec3 position, const Cube &bounds, float timePeriod,
                       vec3 colour, float power)
    : Light(position, bounds, timePeriod, colour, power, 1) {}

vector<Ray> PointLight::calculateRays(vec3 target) const {
  vector<Ray> rays;
  rays.emplace_back(position, target - position);
  return rays;
}
