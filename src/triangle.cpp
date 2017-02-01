#include "triangle.h"

Triangle::Triangle(vec3 v0, vec3 v1, vec3 v2, vec3 color)
	: v0(v0), v1(v1), v2(v2), normal(getNormal(v0, v1, v2)), color(color) {
}

vec3 Triangle::getNormal(vec3 v0, vec3 v1, vec3 v2) {
	vec3 e1 = v1 - v0;
	vec3 e2 = v2 - v0;
	return glm::normalize(glm::cross(e2, e1));
}
