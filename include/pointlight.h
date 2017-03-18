#include "light.h"
class PointLight : public Light {
public:
  const float speed = 10.0f;

  PointLight(vec3 position, vec3 color, float power);

  bool update(float dt) override;

  void calculateRays(vector<Ray> &rays, vec3 target) const override;

  vec3 directLight(const Ray &ray) const override;

  vec3 vertexLight(Vertex v) const override;
};
