#ifdef useCL
#include "raytracer_cl.h"

RayTracerCL::RayTracerCL(int width, int height, shared_ptr<LightingEngine> lighting,
	shared_ptr<PointLight> light,
	const shared_ptr<const vector<Triangle>> triangles, const shared_ptr<BoundingVolume> boundingVolume,
	bool fullscreen)
	: SdlScreen(width, height, fullscreen), triangles(triangles),
	camera(vec3(277.5f, 277.5f, -480.64), 0.0f, 30.0f), light(light),
	lighting(lighting), boundingVolume(boundingVolume) {

	boilerPlate(width, height);
	int err = 0;
	queue.enqueueWriteBuffer(triangleBuffer, CL_TRUE, 0, sizeof(TriangleStruct) * triangles->size(), cl_triangles);

	cl_float3 lightLoc = { 300.0f, 400.0f, 100.0f };
	queue.enqueueWriteBuffer(lightBuffer, CL_TRUE, 0, sizeof(cl_float3), &lightLoc);
	castRays = cl::Kernel(program, "castRays", &err);
	if (err != 0) {
		cout << "error creating kernel: " << err << "\n";
		exit(1);
	}
	castRays.setArg(0, triangleBuffer);
	castRays.setArg(2, pointBuffer);
	castRays.setArg(4, static_cast<int>(triangles->size()));
	castRays.setArg(5, (cl_int)width);
	castRays.setArg(6, (cl_int)height);
	shader = cl::Kernel(program, "pathTrace", &err);
	if (err != 0) {
		cout << "error creating kernel: " << err << "\n";
		exit(1);
	}
	shader.setArg(0, triangleBuffer);
	shader.setArg(2, pointBuffer);
	shader.setArg(3, imageBuffer);
	shader.setArg(4, static_cast<int>(triangles->size()));
	shader.setArg(5, (cl_int)width);
	shader.setArg(6, (cl_int)height);
	shader.setArg(7, randBuffer);

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
	rands = (cl_float*)malloc(width*height * sizeof(cl_float));
	triangleBuffer = cl::Buffer(context, CL_MEM_READ_ONLY, sizeof(TriangleStruct) * triangles->size());
	imageBuffer = cl::Buffer(context, CL_MEM_READ_WRITE, sizeof(cl_float3)* width*height);
	pointBuffer = cl::Buffer(context, CL_MEM_READ_WRITE, sizeof(RayStruct)* width*height);
	cameraBuffer = cl::Buffer(context, CL_MEM_READ_ONLY, sizeof(cl_float)*(4+9));
	randBuffer = cl::Buffer(context, CL_MEM_READ_ONLY, sizeof(float)* width*height);

}

void RayTracerCL::update(float dt) {
	light->update(dt);
	camera.update(dt);
}

void RayTracerCL::draw(int width, int height) {
#pragma omp parallel for
	for (int y = 0; y < height; ++y) {
		for (int x = 0; x < width; ++x) {
			rands[(y*height + x)] = RAND;
		}
	}
	int err = queue.enqueueWriteBuffer(cameraBuffer, CL_TRUE, 0, sizeof(float) * width*height, rands);

	cl_float viewOffset = (cl_float)camera.viewOffset;
	cameraArray[0] = camera.position.x;
	cameraArray[1] = camera.position.y;
	cameraArray[2] = camera.position.z;
	cameraArray[3] = viewOffset;

	for (int i = 0; i < 3; i++) {
		cameraArray[4 + i * 3] = camera.rotation[i].x;
		cameraArray[4 + i * 3 + 1] = camera.rotation[i].y;
		cameraArray[4 + i * 3 + 2] = camera.rotation[i].z;
	}

	cl_float3 rotation[3];
	rotation[0] = vecToFloat(camera.rotation[0]);
	rotation[1] = vecToFloat(camera.rotation[1]);
	rotation[2] = vecToFloat(camera.rotation[2]);
	err = queue.enqueueWriteBuffer(cameraBuffer, CL_TRUE, 0, sizeof(cl_float)*13, cameraArray);

	if (err != 0) {
		cout << "err: " << err << "\n";
		exit(1);
	}


	cl_float3 lightLoc = vecToFloat(light->position);
	
	castRays.setArg(1, lightLoc);
	castRays.setArg(3, cameraBuffer);
	shader.setArg(1, lightLoc);



	queue.enqueueNDRangeKernel(castRays, cl::NullRange, cl::NDRange((size_t)width, (size_t)height), cl::NullRange);
	err = queue.enqueueNDRangeKernel(shader, cl::NullRange, cl::NDRange((size_t)width, (size_t)height), cl::NullRange);

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
void RayTracerCL::boilerPlate(int width, int height) {

	std::string macros = "";
	int err;
	//ANNOYING OPENCL BOILERPLATE
	cl::Platform::get(&all_platforms);
	int c = 0;
	if (all_platforms.size() == 0) {
		std::cout << " No platforms found. Check OpenCL installation!\n";
		exit(1);
	}
	else if (all_platforms.size() > 1) {
		for (int i = 0; i < all_platforms.size(); i++) {
			std::cout << "platform " << i << " : " << all_platforms[i].getInfo<CL_PLATFORM_NAME>() << "\n";
		}
		c = getchar() - '0';


	}
	default_platform = all_platforms[c];
	std::cout << "Using platform: " << default_platform.getInfo<CL_PLATFORM_NAME>() << "\n";

	if (c == 0) {
		macros = "#define M_PI 3.14159265359f  ";
	}

	default_platform.getDevices(CL_DEVICE_TYPE_ALL, &all_devices);
	c = 0;
	if (all_devices.size() == 0) {
		std::cout << " No devices found. Check OpenCL installation!\n";
		exit(1);
	}
	else if (all_devices.size() > 1) {
		for (int i = 0; i < all_devices.size(); i++) {
			std::cout << "device " << i << " : " << all_devices[i].getInfo<CL_DEVICE_NAME>() << "\n";
		}
		c = getchar() - '0';
	}
	default_device = all_devices[0];
	std::cout << "Using device: " << default_device.getInfo<CL_DEVICE_NAME>() << "\n";
	context = cl::Context({ default_device });

	std::ifstream sourceFile("raytracer.cl");
	sourceCode = macros + std::string(
		std::istreambuf_iterator<char>(sourceFile),
		(std::istreambuf_iterator<char>()));

	sources.push_back(std::make_pair(sourceCode.c_str(), sourceCode.size()));
	cout << "loaded kernel length:" << sourceCode.size();
	program = cl::Program(context, sources, &err);

	if (err != 0) {
		cout << "error creating program: " << err << "\n";
	}
	char* options = "-Werror -cl-fast-relaxed-math";
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


}


#endif