#include "brdf.h"
BRDF::BRDF(vec3 diffuse) : diffuse(diffuse) {};

using glm::length;

float angleBetween(vec3 a, vec3 b) {
	return acosf(dot(a, b) / length(a)*length(b));
}

//attempt at disney BRDF model https://disney-animation.s3.amazonaws.com/library/s2012_pbs_disney_brdf_notes_v2.pdf
vec3 BRDF::getLight(vec3 incident, vec3 view, vec3 normal) {
	vec3 h = (incident + view) / glm::length(incident + view);
	float thetaL = angleBetween(incident, normal);
	float thetaV = angleBetween(view, normal);
	float thetaH = angleBetween(normal, h);
	float thetaD = angleBetween(incident, h);
	return diffuse + (mdf(thetaH)*frc(thetaD)*sf(thetaL, thetaD)) * (1.f/ 4.f*cos(thetaL)*cos(thetaV));
}


//microfacet distribution function
vec3 BRDF::mdf(float angle) {
	return vec3(0, 0, 0);
}

//fresnel reflection coefficient
vec3 BRDF::frc(float angle) {
	return vec3(0, 0, 0);
}

//shadowing factor
vec3 BRDF::sf(float incidence, float view) {
	return vec3(0, 0, 0);
}

