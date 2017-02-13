#include "raytracer_cl.h"

RayTracerCL::RayTracerCL(int width, int height, shared_ptr<LightingEngine> lighting,
	shared_ptr<PointLight> light,
	const shared_ptr<const vector<Triangle>> triangles, const shared_ptr<BoundingVolume> boundingVolume,
	bool fullscreen)
	: SdlScreen(width, height, fullscreen), triangles(triangles),
	camera(vec3(277.5f, 277.5f, -480.64), 0.0f, 30.0f), light(light),
	lighting(lighting), boundingVolume(boundingVolume) {

	int err;
	//ANNOYING OPENCL BOILERPLATE
	cl::Platform::get(&all_platforms);
	if (all_platforms.size() == 0) {
		std::cout << " No platforms found. Check OpenCL installation!\n";
		exit(1);
	}
	default_platform = all_platforms[0];
	std::cout << "Using platform: " << default_platform.getInfo<CL_PLATFORM_NAME>() << "\n";

	
	default_platform.getDevices(CL_DEVICE_TYPE_ALL, &all_devices);
	if (all_devices.size() == 0) {
		std::cout << " No devices found. Check OpenCL installation!\n";
		exit(1);
	}
	default_device = all_devices[0];
	std::cout << "Using device: " << default_device.getInfo<CL_DEVICE_NAME>() << "\n";
	context = cl::Context({ default_device });
	
	std::ifstream sourceFile("raytracer.cl");
	sourceCode = std::string(
		std::istreambuf_iterator<char>(sourceFile),
		(std::istreambuf_iterator<char>()));

	sources.push_back(std::make_pair(sourceCode.c_str(), sourceCode.size() ));
	cout << "loaded kernel length:" << sourceCode.size();
	program = cl::Program(context, sources, &err);

	if (err != 0) {
		cout << "error creating program: " << err << "\n";
	}
	if (program.build({ default_device }) != CL_SUCCESS) {
		std::cout << " Error building: " << program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(default_device) << "\n";
		exit(1);
	}
	create_global_memory(width, height);
	queue = cl::CommandQueue(context, default_device, 0Ui64, &err);
	if (err != 0) {
		cout << "error creating queue: " << err << "\n";
	}
	queue.enqueueWriteBuffer(triangleBuffer, CL_TRUE, 0, sizeof(TriangleStruct) * triangles->size(), cl_triangles);
	float cameraLoc[3] = { 300.0f, 400.0f, 100.0f };
	queue.enqueueWriteBuffer(lightBuffer, CL_TRUE, 0, sizeof(float) * 3, cameraLoc);
	kernel_draw = cl::Kernel(program, "getPixel", &err);
	if (err != 0) {
		cout << "error creating kernel: " << err << "\n";
	}

}


void RayTracerCL::create_global_memory(int width, int height) {
	cl_triangles = (TriangleStruct*)malloc(triangles->size() * sizeof(TriangleStruct));
	vector<Triangle> tris = *triangles;
	for (int i = 0; i < triangles->size(); i++) {
		float v0[3] = { tris[i].v0.x, tris[i].v0.y, tris[i].v0.z };
		float v1[3] = { tris[i].v1.x, tris[i].v1.y, tris[i].v1.z };
		float v2[3] = { tris[i].v2.x, tris[i].v2.y, tris[i].v2.z };
		float c[3] = { tris[i].colour.x, tris[i].colour.y, tris[i].colour.z };
		cl_triangles[i] = { v0, v1, v2, c };
	}
	image = (float*)malloc(width*height * 3 * sizeof(float));
	triangleBuffer = cl::Buffer(context, CL_MEM_READ_WRITE, sizeof(TriangleStruct) * triangles->size());
	lightBuffer = cl::Buffer(context, CL_MEM_READ_WRITE, sizeof(float) * 3);
	imageBuffer = cl::Buffer(context, CL_MEM_READ_WRITE, sizeof(float) * 3 * width*height);
}



void RayTracerCL::update(float dt) {
	light->update(dt);
	camera.update(dt);
}

void RayTracerCL::draw(int width, int height) {
	kernel_draw.setArg(0, triangleBuffer);
	kernel_draw.setArg(1, lightBuffer);
	kernel_draw.setArg(2, imageBuffer);
	kernel_draw.setArg(3, width);
	kernel_draw.setArg(4, height);

	printf("enqueuing buffer");
	queue.enqueueNDRangeKernel(kernel_draw, cl::NullRange, cl::NDRange(width, height), cl::NullRange);
	queue.enqueueReadBuffer(imageBuffer, CL_TRUE, 0, sizeof(float) * 3 * width*height, image );

#pragma omp parallel for
	for (int y = 0; y < height; ++y) {
		for (int x = 0; x < width; ++x) {
			vec3 lightColour(image[3 * (x*height + y)], image[3 * (x*height + y) + 1], image[3 * (x*height + y) + 2]);
				drawPixel(x, y, vec3(std::min(lightColour.r, 1.0f),
					std::min(lightColour.g, 1.0f),
					std::min(lightColour.b, 1.0f)));
			}
		}
	//queue.finish();
}
