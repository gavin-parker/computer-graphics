#pragma once

#include <vector>

#include "lerp.h"
#include "sdlscreen.h"

class RayTracer: public SdlScreen {
private:


protected:
	void update(float dt) override;
	void draw() override;

public:
	RayTracer(int width, int height, bool fullscreen = false);
};
