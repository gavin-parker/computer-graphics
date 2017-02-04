#include "testmodel.h"

vector<Triangle> loadTestModel()
{
	vector<Triangle> triangles;
	TextureLoader textureLoader = TextureLoader();
	vector<unsigned char> texture;
	if(textureLoader.LoadTexture("texture.png", texture)){
		std::cout << "loaded a texture of size:" << texture.size();
	}
	// Defines colors:
	vec3 red(    0.75f, 0.15f, 0.15f );
	vec3 yellow( 0.75f, 0.75f, 0.15f );
	vec3 green(  0.15f, 0.75f, 0.15f );
	vec3 cyan(   0.15f, 0.75f, 0.75f );
	vec3 blue(   0.15f, 0.15f, 0.75f );
	vec3 purple( 0.75f, 0.15f, 0.75f );
	vec3 white(  0.75f, 0.75f, 0.75f );


	//defining UVs

	vec2 bl(0,128);
	vec2 br(128,128);
	vec2 tl(0,0);
	vec2 tr(128,0);

	triangles.clear();
	triangles.reserve( 5*2*3 );

	// ---------------------------------------------------------------------------
	// Room

	float L = 555;			// Length of Cornell Box side.

	vec3 A(L,0,0);
	vec3 B(0,0,0);
	vec3 C(L,0,L);
	vec3 D(0,0,L);

	vec3 E(L,L,0);
	vec3 F(0,L,0);
	vec3 G(L,L,L);
	vec3 H(0,L,L);


	//UV ordering = corner, width, height

	// Floor:
	triangles.push_back( Triangle( C, B, A, green, texture, mat3x2(tl, tr, br) ) );
	triangles.push_back( Triangle( C, D, B, green, texture, mat3x2(tl, br, bl) ) );

	// Left wall
	triangles.push_back( Triangle( A, E, C, purple, texture, mat3x2(tl, br, tr) ) );
	triangles.push_back( Triangle( C, E, G, purple, texture, mat3x2(tl, bl, br) ) );

	// Right wall
	triangles.push_back( Triangle( F, B, D, yellow, texture, mat3x2(tl, br, tr) ) );
	triangles.push_back( Triangle( H, F, D, yellow, texture, mat3x2(tl, bl, br) ) );

	// Ceiling
	triangles.push_back( Triangle( E, F, G, cyan, texture, mat3x2(tl, br, tr) ) );
	triangles.push_back( Triangle( F, H, G, cyan, texture, mat3x2(tl, bl, br) ) );

	// Back wall
	triangles.push_back( Triangle( G, D, C, white, texture, mat3x2(tl, br, tr) ) );
	triangles.push_back( Triangle( G, H, D, white, texture, mat3x2(tl, bl, br) ) );

	// ---------------------------------------------------------------------------
	// Short block

	A = vec3(290,0,114);
	B = vec3(130,0, 65);
	C = vec3(240,0,272);
	D = vec3( 82,0,225);

	E = vec3(290,165,114);
	F = vec3(130,165, 65);
	G = vec3(240,165,272);
	H = vec3( 82,165,225);

	// Front
	triangles.push_back( Triangle(E,B,A,red, texture, mat3x2(tl, br, tr)) );
	triangles.push_back( Triangle(E,F,B,red, texture, mat3x2(tl, bl, br)) );

	// Front
	triangles.push_back( Triangle(F,D,B,red, texture, mat3x2(tl, br, tr)) );
	triangles.push_back( Triangle(F,H,D,red, texture, mat3x2(tl, bl, br)) );

	// BACK
	triangles.push_back( Triangle(H,C,D,red, texture, mat3x2(tl, br, tr)) );
	triangles.push_back( Triangle(H,G,C,red, texture, mat3x2(tl, bl, br)) );

	// LEFT
	triangles.push_back( Triangle(G,E,C,red, texture, mat3x2(tl, br, tr)) );
	triangles.push_back( Triangle(E,A,C,red, texture, mat3x2(tl, bl, br)) );

	// TOP
	triangles.push_back( Triangle(G,F,E,red, texture, mat3x2(tl, br, tr)) );
	triangles.push_back( Triangle(G,H,F,red, texture, mat3x2(tl, bl, br)) );

	// ---------------------------------------------------------------------------
	// Tall block

	A = vec3(423,0,247);
	B = vec3(265,0,296);
	C = vec3(472,0,406);
	D = vec3(314,0,456);

	E = vec3(423,330,247);
	F = vec3(265,330,296);
	G = vec3(472,330,406);
	H = vec3(314,330,456);

	// Front
	triangles.push_back( Triangle(E,B,A,blue, texture, mat3x2(tl, br, tr)) );
	triangles.push_back( Triangle(E,F,B,blue, texture, mat3x2(tl, bl, br)) );

	// Front
	triangles.push_back( Triangle(F,D,B,blue, texture, mat3x2(tl, br, tr)) );
	triangles.push_back( Triangle(F,H,D,blue, texture, mat3x2(tl, bl, br)) );

	// BACK
	triangles.push_back( Triangle(H,C,D,blue, texture, mat3x2(tl, br, tr)) );
	triangles.push_back( Triangle(H,G,C,blue, texture, mat3x2(tl, bl, br)) );

	// LEFT
	triangles.push_back( Triangle(G,E,C,blue, texture, mat3x2(tl, br, tr)) );
	triangles.push_back( Triangle(E,A,C,blue, texture, mat3x2(tl, bl, br)) );

	// TOP
	triangles.push_back( Triangle(G,F,E,blue, texture, mat3x2(tl, br, tr)) );
	triangles.push_back( Triangle(G,H,F,blue, texture, mat3x2(tl, bl, br)) );

	return triangles;
}
