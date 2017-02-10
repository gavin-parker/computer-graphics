#include "lightingengine.h"
#ifndef unix
#define RAND float(rand()) / RAND_MAX
#else
#define RAND drand48()
#endif


class GlobalIllumination : public LightingEngine {
private:


  vec3 trace(Ray ray, int bounces);

  bool ClosestIntersection(Ray &ray);
  bool anyIntersection(Ray &ray, Ray &surface);
  int sampleCount = 10;
  int total_bounces = 2;
  vec3 environment = vec3(1, 1, 1)*0.2f;
  const shared_ptr<const Cube> boundingBox;

protected:
public:
  GlobalIllumination();
  GlobalIllumination(Scene scene, int sampleCount);

  vec3 calculateLight(Ray ray) final override;
};
