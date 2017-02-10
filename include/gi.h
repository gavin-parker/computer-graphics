#include "lightingengine.h"
#ifndef unix
#define RAND float(rand()) / RAND_MAX
#else
#define RAND drand48()
#endif


class GlobalIllumination : public LightingEngine {
private:
  // const shared_ptr<const vector<Triangle>> triangles;
  // PointLight light;

  vec3 trace(Ray ray, int bounces);

  bool ClosestIntersection(Ray &ray);
  bool anyIntersection(Ray &ray, Ray &surface);
  int sampleCount = 100;

protected:
public:
  GlobalIllumination();
  GlobalIllumination(Scene scene);

  virtual vec3 calculateLight(Ray ray) override;
};
