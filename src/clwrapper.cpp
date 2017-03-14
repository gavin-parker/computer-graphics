#include "clwrapper.h"


Accelerator getGPU(int triangleCount) {
	Accelerator gpu;
	std::vector<cl::Device> all_devices;
	std::vector<cl::Platform> all_platforms;
	std::string sourceCode;
	cl::Program::Sources sources;

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
	gpu.platform = all_platforms[c];
	std::cout << "Using platform: " << gpu.platform.getInfo<CL_PLATFORM_NAME>() << "\n";

	if (c == 0) {
		macros = "#define M_PI 3.14159265359f  \n";
	}
	macros = macros + "#define TRIANGLE_COUNT" + std::to_string(triangleCount);

	gpu.platform.getDevices(CL_DEVICE_TYPE_ALL, &all_devices);
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
	gpu.device = all_devices[0];
	std::cout << "Using device: " << gpu.device.getInfo<CL_DEVICE_NAME>() << "\n";
	gpu.context = cl::Context({ gpu.device });

	std::ifstream sourceFile("raytracer.cl");
	sourceCode = macros + std::string(
		std::istreambuf_iterator<char>(sourceFile),
		(std::istreambuf_iterator<char>()));

	sources.push_back(std::make_pair(sourceCode.c_str(), sourceCode.size()));
	cout << "loaded kernel length:" << sourceCode.size();
	gpu.program = cl::Program(gpu.context, sources, &err);

	if (err != 0) {
		cout << "error creating program: " << err << "\n";
	}
	char* options = " -Werror -cl-fast-relaxed-math -cl-mad-enable -cl-unsafe-math-optimizations -cl-denorms-are-zero";
	if (gpu.program.build({ gpu.device }, options) != CL_SUCCESS) {
		std::cout << " Error building: " << gpu.program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(gpu.device) << "\n";
		exit(1);
	}
	//create_global_memory(width, height);
	gpu.queue = cl::CommandQueue(gpu.context, gpu.device, 0Ui64, &err);
	if (err != 0) {
		cout << "error creating queue: " << err << "\n";
		exit(1);
	}
	return gpu;
}