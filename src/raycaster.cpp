#ifdef useCL
#include "raycaster.h"
RayCaster::RayCaster(const shared_ptr<const vector<Triangle>> triangles, const shared_ptr<BoundingVolume> boundingVolume, bool shittyDrivers) : triangles(triangles), boundingVolume(boundingVolume), shittyDrivers(shittyDrivers) {
	gpu = getGPU(triangles->size());
	cl_float3* cl_triangles = (cl_float3*)malloc(triangles->size() * sizeof(cl_float3) * 5);
	vector<Triangle> tris = *triangles;
	for (int i = 0; i < triangles->size(); i++) {
		cl_float3 v0 = { tris[i].v0.x, tris[i].v0.y, tris[i].v0.z };
		cl_float3 v1 = { tris[i].v1.x, tris[i].v1.y, tris[i].v1.z };
		cl_float3 v2 = { tris[i].v2.x, tris[i].v2.y, tris[i].v2.z };
		cl_float3 c = { tris[i].colour.x, tris[i].colour.y, tris[i].colour.z };
		cl_float3 normal = { tris[i].normal.x, tris[i].normal.y, tris[i].normal.z };
		cl_triangles[i] = v0;
		cl_triangles[triangles->size() + i] = v1;
		cl_triangles[triangles->size() * 2 + i] = v2;
		cl_triangles[triangles->size() * 3 + i] = c;
		cl_triangles[triangles->size() * 4 + i] = normal;
	}
	triangleBuffer = cl::Buffer(gpu.context, CL_MEM_READ_ONLY, sizeof(cl_float3) * triangles->size() * 5);
	gpu.queue.enqueueWriteBuffer(triangleBuffer, CL_TRUE, 0, sizeof(cl_float3) * triangles->size() * 5, cl_triangles);
	int err = 0;
	rayCastKernel = cl::Kernel(gpu.program, "fastRayCast", &err);
	rayCastKernel.setArg(0, triangleBuffer	);
	rayBuffer = cl::Buffer(gpu.context, CL_MEM_READ_WRITE, sizeof(RayStruct) * maxRays, NULL, &err);


}

RayStruct clifyRay(Ray ray) {
	RayStruct cl_ray;
	cl_ray.origin = (cl_float3)vecToFloat(ray.position);
	cl_ray.direction = (cl_float3)vecToFloat(ray.direction);
	cl_ray.collision = (cl_int)-1;
	cl_ray.collisionLocation = (cl_float3)vecToFloat(vec3(0, 0, 0));
	cl_ray.length = (cl_float)numeric_limits<float>::max();
	return cl_ray;
}


int RayCaster::enqueueRay(Ray ray) {

	rays.push_back(clifyRay(ray));
	if ((rays.size() % maxRays) == 0 && rays.size() > 0) {
		castRays(false | shittyDrivers, chunkIndex*maxRays, maxRays);

		chunkIndex++;
	}
	return rays.size() - 1;
}

bool RayCaster::flushBuffer() {
	rays.clear();
	chunkIndex = 0;
	return true;
}

int RayCaster::getRayCollision(int index) {
	return (int)rays[index].collision;
}

Triangle RayCaster::getRayTriangleCollision(int index) {
	int triangle = rays[index].collision;
	return (*triangles)[triangle];
}

Ray RayCaster::getRay(int index, bool &anyCollision) {
	RayStruct cl_ray = rays[index];
	Ray ray;
	if (cl_ray.collision > -1) {
		ray.collision = &(*triangles)[cl_ray.collision];
	}
	else {
		anyCollision = false;
	}
	ray.collisionLocation = floatToVec(cl_ray.collisionLocation);
	ray.collisionUVLocation = vec3(0, 0, 0);
	ray.direction = floatToVec(cl_ray.direction);
	ray.length = cl_ray.length;
	ray.position = floatToVec(cl_ray.origin);
	return ray;
}

bool RayCaster::castRays(bool sync, int bottom, int size) {
	if (size == -1) {
		size = rays.size() % maxRays;
		bottom = maxRays * chunkIndex;
		sync = true;
	}
	int err = 0;
	err = gpu.queue.enqueueWriteBuffer(rayBuffer, sync, 0, sizeof(RayStruct) * size, &rays[bottom]);

	if (err != 0) {
		cout << "failed write rays: " << err;
		return false;
	}
	rayCastKernel.setArg(1, rayBuffer);
	err = gpu.queue.enqueueNDRangeKernel(rayCastKernel, 0, cl::NDRange(size), cl::NullRange);
	if (err != 0) {
		cout << "failed to cast rays: " << err;
		return false;
	}

	err = gpu.queue.enqueueReadBuffer(rayBuffer, sync, 0, sizeof(RayStruct) * size, &rays[bottom]);
	if (err != 0) {
		cout << "failed to read rays: " << err;
		return false;
	}


	return true;
}
#else
#include "raycaster.h"
RayCaster::RayCaster(const shared_ptr<const vector<Triangle>> triangles, const shared_ptr<BoundingVolume> boundingVolume) : triangles(triangles), boundingVolume(boundingVolume), rays(vector<RayStruct>(maxRays)) {
}

int RayCaster::enqueueRay(Ray ray) {
	if (bufferPointer >= maxRays) {
		return -1;
	}
	boundingVolume->calculateIntersection(ray, true);
	rays[bufferPointer] = ray;
	bufferPointer++;
	return bufferPointer - 1;
}

int RayCaster::getRayCollision(int index) {
	return (int)rays[index].collision;
}

Triangle RayCaster::getRayTriangleCollision(int index) {
	return (triangles)[rays[index].collision];
}

bool RayCaster::castRays() {
	return true;
}


#endif