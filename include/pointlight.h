#pragma once

#include <iostream>
#include <glm/glm.hpp>
#include <SDL.h>
#include <math.h>
#include <algorithm>
#include "lerp.h"

using glm::vec3;
using glm::mat3;

class PointLight {
private:
	vec3 position;
	vec3 color;

public:
	PointLight(vec3 position, vec3 color);

	vec3 directLight(vec3 point, vec3 normal);
};