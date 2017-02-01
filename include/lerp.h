#pragma once

// Linearly interpolate between a and b
inline float lerp(float a, float b, float t) {
	return a + (b - a) * t;
}

inline float deLerp(float a, float b, float t) {
	return (t - a) / (b - a);
}
