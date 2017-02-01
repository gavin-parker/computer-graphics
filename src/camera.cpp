#include "camera.h"

Camera::Camera(glm::vec3 position, float focalLength):
	position(position),
	focalLength(focalLength) {

}

void Camera::Update(float dt)
{
	//std::cout << "Render time: " << dt << " s" << std::endl;
	Uint8* keystate = SDL_GetKeyState( 0 );
	
	position.x += (keystate[SDLK_d] - keystate[SDLK_a]) * velocity * dt;
	position.y += (keystate[SDLK_s] - keystate[SDLK_w]) * velocity * dt;
}
