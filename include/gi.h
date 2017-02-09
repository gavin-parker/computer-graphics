#include "lightingengine.h"
class GlobalIllumination : public LightingEngine {
private:
  // const shared_ptr<const vector<Triangle>> triangles;
  // PointLight light;

  vec3 trace(Ray ray, int bounces);

  bool ClosestIntersection(Ray &ray);
  bool anyIntersection(Ray &ray, Ray &surface);
  int sampleCount = 5;

protected:
public:
  GlobalIllumination();
  GlobalIllumination(Scene scene);

  virtual vec3 calculateLight(Ray ray) override;
};
