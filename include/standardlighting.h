#include "lightingengine.h"
class StandardLighting : public LightingEngine {
private:
  bool ClosestIntersection(Ray &ray);
  int sampleCount = 5;
  vec3 ambientLight = vec3(0.1f, 0.1f, 0.1f);

protected:
public:
  StandardLighting(const shared_ptr<const vector<Triangle>> triangles,
                   shared_ptr<PointLight> light);
  StandardLighting();
  virtual vec3 calculateLight(Ray ray) override;
};
