#include "sdlscreen.h"

bool SdlScreen::noQuitMessageSDL() {
	SDL_Event e;
	while (SDL_PollEvent(&e))
	{
		switch (e.type) {
		case SDL_QUIT:
			return false;
		case SDL_KEYDOWN:
			if (e.key.keysym.sym == SDLK_ESCAPE) {
				return false;
			}
			break;
		}
	}
	return true;
}

void SdlScreen::drawPixel(int x, int y, glm::vec3 color) {
	if( x < 0 || surface->w <= x || y < 0 || surface->h <= y )
		return;

	Uint8 r = Uint8( glm::clamp( 255*color.r, 0.f, 255.f ) );
	Uint8 g = Uint8( glm::clamp( 255*color.g, 0.f, 255.f ) );
	Uint8 b = Uint8( glm::clamp( 255*color.b, 0.f, 255.f ) );

	Uint32* p = (Uint32*)surface->pixels + y*surface->pitch/4 + x;
	*p = SDL_MapRGB( surface->format, r, g, b );
}

SdlScreen::SdlScreen(int width, int height, bool fullscreen) {
	if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER) < 0) {
		std::cout << "Could not init SDL: " << SDL_GetError() << std::endl;
		exit(EXIT_FAILURE);
	}

	Uint32 flags = SDL_SWSURFACE;
	if (fullscreen) {
		flags |= SDL_FULLSCREEN;
	}

	surface = SDL_SetVideoMode(width, height, 32, flags);

	if (surface == 0)
	{
		std::cout << "Could not set video mode: " << SDL_GetError() << std::endl;
		exit(EXIT_FAILURE);
	}

	time = SDL_GetTicks();
}

SdlScreen::~SdlScreen() {
	SDL_Quit();
}

int SdlScreen::getWidth() {
	return surface->w;
}

int SdlScreen::getHeight() {
	return surface->h;
}

void SdlScreen::run() {
	while (noQuitMessageSDL()) {
		Uint32 newTime = SDL_GetTicks();

		Uint32 dt = newTime - time;

		update(dt / 1000.0f);

		time = newTime;

		SDL_FillRect(surface, 0, 0);

		if (SDL_MUSTLOCK(surface))
			SDL_LockSurface(surface);

		draw();

		if (SDL_MUSTLOCK(surface))
			SDL_UnlockSurface(surface);

		SDL_UpdateRect(surface, 0, 0, 0, 0);
	}
}

void SdlScreen::saveBMP(const char *fileName) {
	SDL_SaveBMP(surface, fileName);
}

