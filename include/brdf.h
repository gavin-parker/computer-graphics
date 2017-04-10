#pragma once
#include <glm/glm.hpp>
#include "stdlib.h"
#include "math.h"
#include "stdio.h"
#include <string>
#include <iostream>
using glm::vec3;
using glm::length;

#define BRDF_SAMPLING_RES_THETA_H       90
#define BRDF_SAMPLING_RES_THETA_D       90
#define BRDF_SAMPLING_RES_PHI_D         360

#define RED_SCALE (1.0/1500.0)
#define GREEN_SCALE (1.15/1500.0)
#define BLUE_SCALE (1.66/1500.0)
#define M_PI	3.1415926535897932384626433832795

class BRDF {
private: 
	double* brdf;

public:
	//returns the reflection
	vec3 getLight(vec3 incident, vec3 view, vec3 normal);
	
	//microfacet distribution function
	vec3 mdf(float angle);

	//fresnel reflection coefficient
	vec3 frc(float angle);

	//shadowing factor
	vec3 sf(float incidence, float view);

	BRDF(const char* filename);

};