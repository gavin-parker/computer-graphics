#include "pointlight.h"

PointLight::PointLight(vec3 position, vec3 color):
    position(position),
    color(color){
}

//Uses equation 27 on https://www.cs.bris.ac.uk/Teaching/Resources/COMS30115/Assignment/2017-COMS30115-1.pdf
//To calculate power of light at an intersection
vec3 PointLight::directLight(vec3 point, vec3 normal){
    vec3 light_direction = glm::normalize(point - position);
    float radius = glm::distance(position, point);
    return (color*std::max(glm::dot(light_direction, normal), 0.0f))/(4.0f*(static_cast<float>(M_PI))*radius*radius);
};