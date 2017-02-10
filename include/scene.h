
class Scene {
private:
public:
  const shared_ptr<PointLight> light;
  const shared_ptr<const vector<Triangle>> triangles;
  Scene(shared_ptr<PointLight> light,
        shared_ptr<const vector<Triangle>> triangles)
      : light(light), triangles(triangles){};
};
