#include "starscreen.h"

int main()
{
	StarScreen screen(500, 500, 1000, 0.5, false);

	screen.run();

	screen.saveBMP("screenshot.bmp");

	return EXIT_SUCCESS;
}

