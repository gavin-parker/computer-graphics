#include <string>

#include "rasteriser.h"
#include "raytracer.h"
#include "starscreen.h"
#ifndef unix
extern "C" {
	FILE __iob_func[3] = { stdin, stdout,*stderr }; 
}
#endif

using std::string;

int main(int argc, char *argv[]) {
  if (argc == 2) {
    string mode(argv[1]);

    // load in a scene
    const shared_ptr<const vector<Triangle>> triangles = loadTestModel();
    shared_ptr<PointLight> light(new PointLight(
        vec3(400.0f, 200.0f, 100.0f), vec3(1.0, 1.0f, 1.0f), 1000000.0f));
    const Scene scene(light, triangles);

    if (mode == "stars") {
      StarScreen screen(500, 500, 1000, 0.5, false);
      screen.run();
      screen.saveBMP("screenshot.bmp");

      return EXIT_SUCCESS;
    } else if (mode == "ray") {

      shared_ptr<LightingEngine> engine(new StandardLighting(scene));
      RayTracer screen(500, 500, engine, light, triangles, false);
      screen.run();
      screen.saveBMP("screenshot.bmp");
    } else if (mode == "rast") {
      Rasteriser screen(500, 500, scene, false);
      screen.run();
      screen.saveBMP("screenshot.bmp");
    } else if (mode == "gi") {

      shared_ptr<LightingEngine> engine(new GlobalIllumination(scene));
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
  return EXIT_SUCCESS;
}
