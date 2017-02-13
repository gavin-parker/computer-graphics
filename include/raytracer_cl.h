#pragma once
#include <limits>
#include <omp.h>
#include <fstream>
#include <CL/cl.hpp>
#include "camera.h"
#include "convergent_gi.h"
#include "cube.h"
#include "pointlight.h"
#include "sdlscreen.h"
#include "testmodel.h"
#include "bvh.h"
struct TriangleStruct {
	float* v0[3];
	float* v1[3];
	float* v2[3];
	float* color[3];
};


class RayTracerCL : public SdlScreen {
private:
	//bool ClosestIntersection(Ray &ray);
	const shared_ptr<const vector<Triangle>> triangles;
	Camera camera;

	const vec3 ambientLight = vec3(0.1f, 0.1f, 0.1f);
	const shared_ptr<PointLight> light;
	shared_ptr<LightingEngine> lighting;
	const shared_ptr<BoundingVolume> boundingVolume;
	void create_global_memory();
	cl::Context context;
	cl::Device default_device;
	TriangleStruct* cl_triangles;
	cl::Buffer triangleBuffer;
	cl::Buffer lightBuffer;
	cl::CommandQueue queue;
	cl::Kernel kernel_draw;
	cl::Program program;
	cl::Program::Sources sources;
	std::vector<cl::Device> all_devices;
	cl::Platform default_platform;
	std::vector<cl::Platform> all_platforms;
	std::string sourceCode;
protected:
	void update(float dt) override;
	void draw(int width, int height) override;

public:
	RayTracerCL(int width, int height, shared_ptr<LightingEngine> lighting,
		const shared_ptr<PointLight> light,
		const shared_ptr<const vector<Triangle>> triangles, const shared_ptr<BoundingVolume> boundingVolume,
		bool fullscreen = false);
};