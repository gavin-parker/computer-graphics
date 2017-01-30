#include <iostream>
#include <glm/glm.hpp>
#include <SDL.h>
#include "SDLauxiliary.h"
#include "TestModel.h"

using namespace std;
using glm::vec3;
using glm::mat3;

/* ----------------------------------------------------------------------------*/
/* GLOBAL VARIABLES                                                            */

const int SCREEN_WIDTH = 500;
const int SCREEN_HEIGHT = 500;
SDL_Surface* screen;
int t;

/* ----------------------------------------------------------------------------*/
/* FUNCTIONS                                                                   */

void Update(vector<vec3>& stars);
void Draw(vector<vec3>& stars);
void Interpolate(float a, float b, vector<float>& result);
void Interpolate(vec3 a, vec3 b, vector<vec3>& result);
void StarField(vector<vec3>& stars);

int main()
{
	screen = InitializeSDL( SCREEN_WIDTH, SCREEN_HEIGHT );
	t = SDL_GetTicks();	// Set start value for timer.
	
	//Test linear interpolation
	vector<float> result(10);
	Interpolate(5, 14, result);
	for(unsigned int i=0; i < result.size(); i++){
		cout << result[i] << " ";
	}
	cout << "\n";

	vector<vec3> stars(1000);
	StarField(stars);

	while( NoQuitMessageSDL() )
	{
		Update(stars);
		Draw(stars);
	}

	SDL_SaveBMP( screen, "screenshot.bmp" );
	return 0;
}

void Update(vector<vec3>& stars)
{
	// Compute frame time:
	int t2 = SDL_GetTicks();
	float dt = float(t2-t);
	t = t2;
	float v = 0.05;
	//calculate star transform
	for(size_t i=0; i < stars.size(); i++){
		stars[i].z -= v*dt;
		if(stars[i].z <= 0){
			stars[i].z += 1;
		}
		if(stars[i].z > 1){
			stars[i].z -= 1;
		}
	}
	cout << "Render time: " << dt << " ms." << endl;
}

void Draw(vector<vec3>& stars)
{
	SDL_FillRect( screen, 0, 0 );
	if( SDL_MUSTLOCK(screen) )
		SDL_LockSurface(screen);

	//calculate projections of stars
	int focal_length = SCREEN_HEIGHT/2;
	for(size_t i=0; i < stars.size(); i++){
		int u = focal_length*(stars[i][0]/stars[i][2]) + SCREEN_WIDTH/2;
		int v = focal_length*(stars[i][1]/stars[i][2]) + SCREEN_HEIGHT/2;
		vec3 color = 0.2f * vec3(1,1,1) / (stars[i].z*stars[i].z);
		PutPixelSDL(screen, u, v, color);
	}

	if( SDL_MUSTLOCK(screen) )
		SDL_UnlockSurface(screen);

	SDL_UpdateRect( screen, 0, 0, 0, 0 );
}

//Linearly interpolate between two values
void Interpolate(float a, float b, vector<float>& result){
	if(result.size() == 1){
		result[0] = (a + b)/2;
		return;
	}
	float step = (b + 1 - a) / result.size();
	for(unsigned int i=0; i < result.size(); i++){
		result[i] = a + step*i;
	}
}

//Linearly interpolate between two vectors
void Interpolate(vec3 a, vec3 b, vector<vec3>& result){
	if(result.size() == 1){
		result[0] = vec3((a[0] + b[0]/2), (a[1] + b[1]/2), (a[2] + b[2]/2));
		return;
	}
	vec3 step((b[0] + 1 - a[0]) / result.size(), (b[1] + 1 - a[1]) / result.size(), (b[2] + 1 - a[2]) / result.size());
	for(unsigned int i = 0; i < result.size(); i++){
		result[i] = a + step* (float)i;
	}
}
//generate random star locations
void StarField(vector<vec3> &stars){
	for(int i=0; i < 1000; i ++){
		float x =  (float(rand()) / float(RAND_MAX))*2.0f - 1;
		float y =  (float(rand()) / float(RAND_MAX))*2.0f - 1;
		float z =  float(rand()) / float(RAND_MAX);
		stars[i] = vec3(x,y,z);
	}

}