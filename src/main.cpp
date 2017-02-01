#include "starscreen.h"
#include "raytracer.h"

int main(int argc, char* argv[])
{
	if (argc == 1) {
		StarScreen screen(500, 500, 1000, 0.5, false);

		screen.run();

		screen.saveBMP("screenshot.bmp");
	} else {
		cout << "running raytracer" << endl;
		RayTracer screen(250, 250, false);
		screen.run();
		screen.saveBMP("screenshot.bmp");
	}

	return EXIT_SUCCESS;
}

