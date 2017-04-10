#pragma once
#include "lightingengine.h"

class BRDF {
private: 
	vec3 diffuse;

public:
	//returns the reflection
	vec3 getLight(vec3 incident, vec3 view, vec3 normal);
	
	//microfacet distribution function
	vec3 mdf(float angle);

	//fresnel reflection coefficient
	vec3 frc(float angle);

	//shadowing factor
	vec3 sf(float incidence, float view);

	BRDF(vec3 diffuse);

};