#include <cstdlib>
#include <ctime>
#include <string>

#include "objects.h"
#include "rasteriser.h"
#include "raytracer.h"
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

    PointLight light(vec3(300.0f, 400.0f, 100.0f), vec3(1.0, 1.0f, 1.0f),
                     1000000.0f);
    SphereLight softLight(vec3(300.0f, 400.0f, 100.0f), vec3(1.0, 1.0f, 1.0f),
                          1000000.0f, 4.0f, 5);

    /*const shared_ptr<const vector<Triangle>> geometry = loadTestModel();
    const shared_ptr<BoundingVolume> cornelBVH = loadTestModelBVH();*/

    Box box;

    Ptr_Triangles geometry = box.allTriangles();

    BoundingVolume boxBVH = box.createBoundingVolume();

    Scene scene = {softLight, geometry, boxBVH};
    Scene scene_low_quality = {light, geometry, boxBVH};

/*TerrainGenerator terrainGen;
const shared_ptr<const vector<Triangle>> terrain =
    terrainGen.generateTerrain(1000, 0, 100, vec3(-100, 0, -100));
shared_ptr<Scene> sceneB(
    new Scene(light, terrain,
              shared_ptr<BoundingVolume>(new BoundingVolume(terrain))));*/

#pragma omp parallel
    {
#pragma omp master
      {
        int threads = omp_get_num_threads();
        cout << "running on " << threads << " threads";
      }
    }

    if (mode == "stars") {
      StarScreen screen(500, 500, 1000, 0.5, false);
      screen.run();
      screen.saveBMP("screenshot.bmp");

      return EXIT_SUCCESS;
    } else if (mode == "ray") {

      StandardLighting engine(scene_low_quality);
      RayTracer screen(500, 500, engine, light, geometry, boxBVH, false);
      screen.run();
      screen.saveBMP("screenshot.bmp");
    } else if (mode == "rast") {
      RastLighting engine(scene_low_quality);
      Rasteriser screen(500, 500, engine, scene_low_quality);
      screen.run();
      screen.saveBMP("screenshot.bmp");
    } else if (mode == "gi") {
      int sampleCount = 10;
      if (argc > 2) {
        string samples(argv[2]);
        sampleCount = atoi(samples.c_str());
      }
      GlobalIllumination engine(scene, sampleCount);
      RayTracer screen(500, 500, engine, light, geometry, boxBVH, false);
      screen.run();
      screen.saveBMP("screenshot.bmp");
    } else if (mode == "conv") {
      int sampleCount = 10;
      if (argc > 2) {
        string samples(argv[2]);
        sampleCount = atoi(samples.c_str());
      }
      ConvergentGlobalIllumination engine(scene, sampleCount, 512, 512);
      RayTracer screen(512, 512, engine, softLight, geometry, boxBVH, false);
      screen.run();
      screen.saveBMP("screenshot.bmp");
    } else if (mode == "baked") {
      // BakedGI engine(scene, 5, 100);
      // Rasteriser screen(500, 500, engine, scene, vec3(277.5f, 277.5f,
      // -480.64), false);
      // screen.run();
      // screen.saveBMP("screenshot.bmp");
    }

#ifdef useCL
    else if (mode == "cl") {
      light.power *= 10;
      StandardLighting engine(scene);
      RayTracerCL screen(1024, 1024, engine, light, geometry, boxBVH);
      screen.run();
      screen.saveBMP("screenshot.bmp");
    }
#endif
    else {
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
