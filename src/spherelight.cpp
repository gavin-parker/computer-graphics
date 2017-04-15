#include "spherelight.h"

SphereLight::SphereLight(vec3 position, const Cube &bounds, float timePeriod,
                         vec3 colour, float power, float radius, int res)
    : Light(position, bounds, timePeriod, colour, power, res), radius(radius) {}

vector<Ray> SphereLight::calculateRays(vec3 target) const {
  vector<Ray> rays;
  rays.reserve(rayCount);
  for (int i = 0; i < rayCount; i++) {
    // pick a random point on the sphere maybe?
    float theta = RAND() * 2 * M_PI;
    float phi = acos(2 * RAND() - 1);
    vec3 point(radius * cos(theta) * sin(phi), radius * sin(theta) * sin(phi),
               radius * cos(phi));
    point += position;
    rays.emplace_back(point, target - point);
  }
  return rays;
}
