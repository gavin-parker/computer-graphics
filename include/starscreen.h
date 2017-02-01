#pragma once

#include <vector>

#include "lerp.h"
#include "sdlscreen.h"

using std::vector;

class StarScreen: public SdlScreen {
private:
	vector<vec3> stars;
	float starVelocity;

protected:
	void update(float dt) override;
	void draw(int width, int height) override;

public:
	StarScreen(int width, int height, vector<vec3>::size_type starCount, float starVelocity,  bool fullscreen = false);
};
