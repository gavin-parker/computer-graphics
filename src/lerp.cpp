#include "lerp.h"

float lerp(float a, float b, float t) {
	return a + (b - a) * t;
}

void lerp(float a, float b, std::vector<float>& results) {
	if(results.size() == 1) {
		results[0] = lerp(a, b, 0.5);
	} else {
		unsigned int N = results.size() - 1;

		for (unsigned int n = 0; n <= N; ++n){
			results[n] = lerp(a, b, n / N);
		}
	}
}

float deLerp(float a, float b, float t) {
	return (t - a) / (b - a);
}
