#include "lerp.h"

float lerp(float a, float b, float t) {
	return a + (b - a) * t;
}

float deLerp(float a, float b, float t) {
	return (t - a) / (b - a);
}
