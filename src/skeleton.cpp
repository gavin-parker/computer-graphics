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

void Update();
void Draw();
void Interpolate(float a, float b, vector<float>& result);
void Interpolate(vec3 a, vec3 b, vector<vec3>& result);

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

	while( NoQuitMessageSDL() )
	{
		Update();
		Draw();
	}

	SDL_SaveBMP( screen, "screenshot.bmp" );
	return 0;
}

void Update()
{
	// Compute frame time:
	int t2 = SDL_GetTicks();
	float dt = float(t2-t);
	t = t2;
	cout << "Render time: " << dt << " ms." << endl;
}

void Draw()
{
	if( SDL_MUSTLOCK(screen) )
		SDL_LockSurface(screen);

	for( int y=0; y<SCREEN_HEIGHT; ++y )
	{

		for( int x=0; x<SCREEN_WIDTH; ++x )
		{
			//vec3 color( 0.0, 1.0, 0.0 );
			PutPixelSDL( screen, x, y, result[x] );
		}
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