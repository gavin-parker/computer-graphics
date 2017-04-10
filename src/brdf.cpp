#include "brdf.h"
BRDF::BRDF(vec3 diffuse) : diffuse(diffuse) {};


//attempt at disney BRDF model https://disney-animation.s3.amazonaws.com/library/s2012_pbs_disney_brdf_notes_v2.pdf
vec3 BRDF::getLight(vec3 incident, vec3 view) {
	vec3 h = (incident + view) / glm::length(incident + view);



}