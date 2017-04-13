#include <cstdlib>
#include <ctime>
#include <string>

#include "objects.h"
#include "rasteriser.h"
#include "rastlighting.h"
#include "raytracer.h"
#include "standardlighting.h"
#include "starscreen.h"
#include "terrain_gen.h"

#ifdef useCL
#include "raytracer_cl.h"
#endif

#ifndef unix
extern "C" {
FILE __iob_func[3] = {stdin, stdout, *stderr};
}
#endif

int main(int argc, char *argv[]) {
#ifndef unix
  srand(static_cast<unsigned>(time(0)));
#endif
  if (argc >= 2) {
    string mode(argv[1]);

    Box box;

    Ptr_Triangles geometry = box.allTriangles();

    BoundingVolume bvh = box.createBoundingVolume();

    Cube bounds(geometry);

    vec3 lightPosition = lerp(bounds.a, bounds.b, vec3(0.6, 0.8, 0.2));

    vec3 lightColour(1.0f, 1.0f, 1.0f);

    float lightPower = 0.5f * pow(glm::distance(bounds.a, bounds.b), 2.0f);

    PointLight light(lightPosition, lightColour, lightPower);
    SphereLight softLight(lightPosition, lightColour, lightPower, 4.0f, 5);

    Scene scene_low_quality = {light, geometry, bvh, bounds};
    Scene scene = {softLight, geometry, bvh, bounds};

    float viewAngle = 30.0f;

#pragma omp parallel
    {
#pragma omp master
      {
        int threads = omp_get_num_threads();
        cout << "running on " << threads << " threads";
      }
    }

    SdlScreen *screen = nullptr;
    LightingEngine *engine = nullptr;

    if (mode == "stars") {
      screen = new StarScreen(500, 500, 1000, 0.5);
    } else if (mode == "ray") {
      engine = new StandardLighting(scene_low_quality);
      screen =
          new RayTracer(500, 500, viewAngle, 4, *engine, scene_low_quality);
    } else if (mode == "rast") {
      engine = new RastLighting(scene_low_quality);
      screen = new Rasteriser(500, 500, viewAngle, *engine, scene_low_quality);
    } else if (mode == "gi") {
      int sampleCount = 10;
      if (argc > 2) {
        string samples(argv[2]);
        sampleCount = atoi(samples.c_str());
      }
      engine = new GlobalIllumination(scene, sampleCount);
      screen =
          new RayTracer(500, 500, viewAngle, 4, *engine, scene_low_quality);
    } else if (mode == "conv") {
      int sampleCount = 10;
      if (argc > 2) {
        string samples(argv[2]);
        sampleCount = atoi(samples.c_str());
      }
      engine = new ConvergentGlobalIllumination(scene, sampleCount, 512, 512);
      screen = new RayTracer(512, 512, viewAngle, 4, *engine, scene);
    }
#ifdef useCL
    else if (mode == "cl") {
      light.power *= 10;
      engine = new StandardLighting(scene);
      screen = new RayTracerCL(1024, 1024, *engine, light, geometry, bvh);
    }
#endif
    else {
      cout << "Unknown mode \"" << mode << "\"" << endl;

      return EXIT_FAILURE;
    }

    screen->run();
    screen->saveBMP("screenshot.bmp");

    delete screen;
    delete engine;

    return EXIT_SUCCESS;
  } else {
    cout << "Please enter a mode:" << endl;
    cout << "\tstars - stars" << endl;
    cout << "\tray - raytracer" << endl;
    cout << "\trast - rasterizer" << endl;
    cout << "\tgi - global illumination" << endl;
    cout << "\tconv - convergent global illumination" << endl;
#ifdef useCL
    cout << "\tcl - openCL raytracer" << endl;
#endif

    return EXIT_FAILURE;
  }
}
