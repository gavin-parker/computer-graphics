#pragma once

#include <iostream>
#include <glm/glm.hpp>
#include <SDL.h>

class SdlScreen {
private:
	SDL_Surface* surface;
	Uint32 time;

	bool noQuitMessageSDL();

protected:
	void drawPixel(int x, int y, glm::vec3 color);

	virtual void update(float dt) = 0;
	virtual void draw() = 0;

public:
	SdlScreen(int width, int height, bool fullscreen = false);

	virtual ~SdlScreen();

	int getWidth();
	int getHeight();

	void run();

	void saveBMP(const char* fileName);
};
