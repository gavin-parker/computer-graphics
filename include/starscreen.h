#pragma once

#include <vector>

#include "lerp.h"
#include "sdlscreen.h"

class StarScreen: public SdlScreen {
private:
	vector<vec3> stars;
	float starVelocity;

protected:
	void update(float dt) override;
	void draw() override;

public:
	StarScreen(int width, int height, std::vector<vec3>::size_type starCount, float starVelocity,  bool fullscreen = false);
};
