#include "raytracer_cl.h"

RayTracerCL::RayTracerCL(int width, int height, shared_ptr<LightingEngine> lighting,
	shared_ptr<PointLight> light,
	const shared_ptr<const vector<Triangle>> triangles, const shared_ptr<BoundingVolume> boundingVolume,
	bool fullscreen)
	: SdlScreen(width, height, fullscreen), triangles(triangles),
	camera(vec3(277.5f, 277.5f, -480.64), 0.0f, 30.0f), light(light),
	lighting(lighting), boundingVolume(boundingVolume), cameraStruct(new CameraStruct()) {

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
	char* options = "-Werror";
	if (program.build({ default_device }, options) != CL_SUCCESS) {
		std::cout << " Error building: " << program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(default_device) << "\n";
		exit(1);
	}
	create_global_memory(width, height);
	queue = cl::CommandQueue(context, default_device, 0Ui64, &err);
	if (err != 0) {
		cout << "error creating queue: " << err << "\n";
		exit(1);

	}
	queue.enqueueWriteBuffer(triangleBuffer, CL_TRUE, 0, sizeof(TriangleStruct) * triangles->size(), cl_triangles);

	cl_float3 lightLoc = { 300.0f, 400.0f, 100.0f };
	queue.enqueueWriteBuffer(lightBuffer, CL_TRUE, 0, sizeof(cl_float3), &lightLoc);
	kernel_draw = cl::Kernel(program, "getPixel", &err);
	if (err != 0) {
		cout << "error creating kernel: " << err << "\n";
		exit(1);
	}

}


void RayTracerCL::create_global_memory(int width, int height) {
	cl_triangles = (TriangleStruct*)malloc(triangles->size() * sizeof(TriangleStruct));
	vector<Triangle> tris = *triangles;
	for (int i = 0; i < triangles->size(); i++) {
		cl_float3 v0 = { tris[i].v0.x, tris[i].v0.y, tris[i].v0.z };
		cl_float3 v1 = { tris[i].v1.x, tris[i].v1.y, tris[i].v1.z };
		cl_float3 v2 = { tris[i].v2.x, tris[i].v2.y, tris[i].v2.z };
		cl_float3 c = { tris[i].colour.x, tris[i].colour.y, tris[i].colour.z };
		cl_float3 normal = { tris[i].normal.x, tris[i].normal.y, tris[i].normal.z };
		cl_triangles[i] = { v0, v1, v2, c, normal };
	}
	image = (cl_float3*)malloc(width*height * sizeof(cl_float3));
	triangleBuffer = cl::Buffer(context, CL_MEM_READ_WRITE, sizeof(TriangleStruct) * triangles->size());
	imageBuffer = cl::Buffer(context, CL_MEM_READ_WRITE, sizeof(cl_float3)* width*height);
	cameraBuffer = cl::Buffer(context, CL_MEM_READ_WRITE, sizeof(CameraStruct));
}



void RayTracerCL::update(float dt) {
	light->update(dt);
	camera.update(dt);
}

void RayTracerCL::draw(int width, int height) {
	cameraStruct->position = { camera.position.x, camera.position.y, camera.position.z };
	cameraStruct->viewOffset = camera.viewOffset;
	cameraStruct->rotation[0] = vecToFloat(camera.rotation[0]);
	cameraStruct->rotation[1] = vecToFloat(camera.rotation[1]);
	cameraStruct->rotation[2] = vecToFloat(camera.rotation[2]);


	queue.enqueueWriteBuffer(cameraBuffer, CL_TRUE, 0, sizeof(CameraStruct), cameraStruct);
	cl_float3 lightLoc = vecToFloat(light->position);


	kernel_draw.setArg(0, triangleBuffer);
	kernel_draw.setArg(1, lightLoc);
	kernel_draw.setArg(2, imageBuffer);
	kernel_draw.setArg(3, cameraBuffer);
	kernel_draw.setArg(4, static_cast<int>(triangles->size()));
	kernel_draw.setArg(5, width);
	kernel_draw.setArg(6, height);

	int err = queue.enqueueNDRangeKernel(kernel_draw, cl::NullRange, cl::NDRange(width, height), cl::NullRange);

	if (err != 0) {
		cout << "err: " << err << "\n";
		exit(1);
	}
	queue.enqueueReadBuffer(imageBuffer, CL_TRUE, 0, sizeof(cl_float3) * width*height, image );
#pragma omp parallel for
	for (int y = 0; y < height; ++y) {
		for (int x = 0; x < width; ++x) {
			cl_float3 pixel = image[(y*height + x)];
			vec3 lightColour(pixel.x, pixel.y, pixel.z);
				drawPixel(x, y, vec3(std::min(lightColour.r, 1.0f),
					std::min(lightColour.g, 1.0f),
					std::min(lightColour.b, 1.0f)));
			}
		}
}
