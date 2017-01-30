#ifndef TRIANGLE_H
#define TRIANGLE_H

#include <glm/glm.hpp>

class Triangle
{
public:
	glm::vec3 v0;
	glm::vec3 v1;
	glm::vec3 v2;
	glm::vec3 normal;
	glm::vec3 color;

	Triangle(glm::vec3 v0, glm::vec3 v1, glm::vec3 v2, glm::vec3 color);

	static glm::vec3 getNormal(glm::vec3 v0, glm::vec3 v1, glm::vec3 v2);
};

#endif // TRIANGLE_H
