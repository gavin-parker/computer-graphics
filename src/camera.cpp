#include "camera.h"

Camera::Camera(vec3 position, float viewAngle):
	position(position),
	viewOffset(static_cast<float>(tan(viewAngle * M_PI / 180.f))){
}

void Camera::Update(float dt)
{
	Uint8* keystate = SDL_GetKeyState( 0 );
	
	position.x += (keystate[SDLK_d] - keystate[SDLK_a]) * velocity * dt;
	position.y += (keystate[SDLK_q] - keystate[SDLK_e]) * velocity * dt;
	position.z += (keystate[SDLK_w] - keystate[SDLK_s]) * velocity * dt;
}

void Camera::calculateRay(Ray &ray, float x, float y)
{
	ray.position = position;
	ray.direction = glm::normalize(vec3(lerp(-viewOffset, viewOffset, x), lerp(-viewOffset, viewOffset, y), 1.0f));
}
