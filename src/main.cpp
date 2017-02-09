#include <string>

#include "rasteriser.h"
#include "raytracer.h"
#include "starscreen.h"
using std::string;

int main(int argc, char *argv[]) {
  if (argc == 2) {
    string mode(argv[1]);

    if (mode == "stars") {
      StarScreen screen(500, 500, 1000, 0.5, false);
      screen.run();
      screen.saveBMP("screenshot.bmp");

      return EXIT_SUCCESS;
    } else if (mode == "ray") {

      const shared_ptr<const vector<Triangle>> triangles = loadTestModel();
      shared_ptr<PointLight> light(new PointLight(
          vec3(400.0f, 200.0f, 100.0f), vec3(1.0, 1.0f, 1.0f), 1000000.0f));
      shared_ptr<LightingEngine> engine(new StandardLighting(triangles, light));
      RayTracer screen(500, 500, engine, light, triangles, false);
      screen.run();
      screen.saveBMP("screenshot.bmp");
    } else if (mode == "rast") {
      Rasteriser screen(500, 500, false);
      screen.run();
      screen.saveBMP("screenshot.bmp");
    } else if (mode == "gi") {
      const shared_ptr<const vector<Triangle>> triangles = loadTestModel();
      shared_ptr<PointLight> light(new PointLight(
          vec3(400.0f, 200.0f, 100.0f), vec3(1.0, 1.0f, 1.0f), 1000000.0f));
      shared_ptr<LightingEngine> engine(
          new GlobalIllumination(triangles, light));
      RayTracer screen(500, 500, engine, light, triangles, false);
      screen.run();
      screen.saveBMP("screenshot.bmp");
    } else {
      cout << "Unknown mode \"" << mode << "\"" << endl;
    }
  } else {
    cout << "Please enter a mode:" << endl
         << "\tstars - starts" << endl
         << "\tray - raytracer" << endl
        //<< "\trast - rasterizer" << endl
        ;
  }
}
