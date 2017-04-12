#include "sdlscreen.h"

bool SdlScreen::noQuitMessageSDL() {
  SDL_Event e;
  while (SDL_PollEvent(&e)) {
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

void SdlScreen::drawPixel(int x, int y, vec3 colour) {
  if (x < 0 || surface->w <= x || y < 0 || surface->h <= y)
    return;

  Uint8 r = Uint8(glm::clamp(255 * colour.r, 0.f, 255.f));
  Uint8 g = Uint8(glm::clamp(255 * colour.g, 0.f, 255.f));
  Uint8 b = Uint8(glm::clamp(255 * colour.b, 0.f, 255.f));

  Uint32 *p = (Uint32 *)surface->pixels + y * surface->pitch / 4 + x;
  *p = SDL_MapRGB(surface->format, r, g, b);
}

SdlScreen::SdlScreen(int width, int height, bool fullscreen) {
  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER) < 0) {
    cout << "Could not init SDL: " << SDL_GetError() << endl;
    exit(EXIT_FAILURE);
  }

  Uint32 flags = SDL_SWSURFACE;
  if (fullscreen) {
    flags |= SDL_FULLSCREEN;
  }

  surface = SDL_SetVideoMode(width, height, 32, flags);

  if (surface == 0) {
    cout << "Could not set video mode: " << SDL_GetError() << endl;
    exit(EXIT_FAILURE);
  }

  time = SDL_GetTicks();
}

SdlScreen::~SdlScreen() { SDL_Quit(); }

void SdlScreen::run() {
  while (noQuitMessageSDL()) {
    const Uint32 newTime = SDL_GetTicks();

    const Uint32 dt = newTime - time;

    update(dt / 1000.0f);

    time = newTime;

    SDL_FillRect(surface, 0, 0);

    if (SDL_MUSTLOCK(surface))
      SDL_LockSurface(surface);

    const Uint32 drawTime = SDL_GetTicks();

    draw(surface->w, surface->h);

    cout << setfill('0') << setw(5) << (SDL_GetTicks() - drawTime) << "ms ";
    cout << (1000.f / (SDL_GetTicks() - drawTime)) << "fps\n";

    if (SDL_MUSTLOCK(surface))
      SDL_UnlockSurface(surface);

    SDL_UpdateRect(surface, 0, 0, 0, 0);
  }
}

void SdlScreen::saveBMP(const char *fileName) {
  SDL_SaveBMP(surface, fileName);
}
