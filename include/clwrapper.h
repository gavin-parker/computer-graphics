#include <CL/cl.hpp>
#include <stdio.h>
#include <iostream>
#include <fstream>

using std::cout;
typedef struct Accelerator {
	cl::CommandQueue queue;
	cl::Device device;
	cl::Context context;
	cl::Program program;
	cl::Platform platform;
}Accelerator;

Accelerator getGPU(int triangleCount);