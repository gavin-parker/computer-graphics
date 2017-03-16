#pragma once
#include <CL/cl.hpp>
#include <stdio.h>
#include <iostream>
#include <fstream>
#include <glm\glm.hpp>

using std::cout;
using glm::vec3;
typedef struct Accelerator {
	cl::CommandQueue queue;
	cl::Device device;
	cl::Context context;
	cl::Program program;
	cl::Platform platform;
}Accelerator;

#pragma pack(1) 
typedef struct RayStruct {
	cl_float3 origin;
	cl_float3 direction;
	cl_float3 collisionLocation;
	cl_float2 dummy;
	cl_float length;
	cl_int collision;
} RayStruct;
#pragma pack() 

inline cl_float3 vecToFloat(vec3 vec) {
	cl_float3 f;
	f.x = vec.x;
	f.y = vec.y;
	f.z = vec.z;
	return f;
}
inline vec3 floatToVec(cl_float3 clf) {
	vec3 v;
	v.x = clf.x;
	v.y = clf.y;
	v.z = clf.z;
	return v;
}



Accelerator getGPU(int triangleCount);