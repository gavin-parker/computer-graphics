#include "lightingengine.h"
class GlobalIllumination : public LightingEngine {
private:
  // const shared_ptr<const vector<Triangle>> triangles;
  // PointLight light;

  vec3 trace(Ray ray, int bounces);

  bool ClosestIntersection(Ray &ray);
  int sampleCount = 5;

protected:
public:
  GlobalIllumination(const shared_ptr<const vector<Triangle>> triangles,
                     shared_ptr<PointLight> light);
  GlobalIllumination();
  virtual vec3 calculateLight(Ray ray) override;
};
