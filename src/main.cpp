#include <string>

#include "starscreen.h"
#include "raytracer.h"

using std::string;

int main(int argc, char* argv[]) {
	if (argc == 2) {
		string mode(argv[1]);

		if (mode == "stars") {
			StarScreen screen(500, 500, 1000, 0.5, false);
			screen.run();
			screen.saveBMP("screenshot.bmp");

			return EXIT_SUCCESS;
		} else if (mode == "ray") {
			RayTracer screen(250, 250, false);
			screen.run();
			screen.saveBMP("screenshot.bmp");
		} else {
			cout << "Unknown mode \"" << mode << "\"" << endl;
		}
	} else {
		cout
				<< "Please enter a mode:" << endl
				<< "\tstars - starts" << endl
				<< "\tray - raytracer" << endl
				//<< "\trast - rasterizer" << endl
				;

	}
}

