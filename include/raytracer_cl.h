#pragma once

#include <CL/cl.hpp>
#include <fstream>
#include <limits>
#include <omp.h>
#include "clwrapper.h"
#include "bvh.h"
#include "camera.h"
#include "convergent_gi.h"
#include "sdlscreen.h"
#include "bvh.h"
#ifdef unix
#include <sys/resource.h>
#include <sys/time.h>
#else
#include <time.h>
#endif

struct TriangleStruct {
  cl_float3 v0;
  cl_float3 v1;
  cl_float3 v2;
  cl_float3 color;
  cl_float3 normal;
};

#pragma pack(1)
typedef struct CameraStruct {
  cl_float viewOffset;
  cl_float3 position;
  // cl_float3 rotation[3];
} CameraStruct;


#pragma pack()


class RayTracerCL : public SdlScreen {
private:
  // bool ClosestIntersection(Ray &ray);
  const vector<Triangle> &triangles;
  Camera camera;

	const vec3 ambientLight = vec3(0.1f, 0.1f, 0.1f);
	PointLight &light;
	LightingEngine &lighting;
	const BoundingVolume &boundingVolume;
	void create_global_memory(int width, int height);
	vector<vec3> averageImage;
	bool refresh = false;
	int frameCounter = 1; 
	Accelerator gpu;
	cl_float3* cl_triangles;
	cl::Buffer triangleBuffer;
	cl::Buffer lightBuffer; 
	cl::Buffer imageBuffer;
	cl::Buffer cameraBuffer;
	cl::Buffer pointBuffer;
	cl::Buffer randBuffer;
	cl::Buffer iCantBelieveItsNotBuffer;
	cl::Kernel castRays;
	cl::Kernel shader;
	std::string sourceCode;
	cl_float3* image;
	cl_uint* rands;
	CameraStruct cameraStruct;
	cl_float cameraArray[4] = { 0,0,0,0 };

protected:
	void update(float dt) override;
	void draw(int width, int height) override;
public:
	RayTracerCL(int width, int height, LightingEngine &lighting,
		PointLight &light,
		const vector<Triangle> &triangles, const BoundingVolume &boundingVolume, vec3 cameraPos = vec3(277.5f, 277.5f, -480.64),
		bool fullscreen = false);
};
