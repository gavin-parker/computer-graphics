#pragma once
#include "ray.h"
#include "bvh.h"
#ifdef useCL
#include "clwrapper.h"
#endif



class RayCaster {
private:
	bool shittyDrivers;
#ifdef useCL
	Accelerator gpu;
	cl::Kernel rayCastKernel;
	cl::Buffer triangleBuffer;
	cl::Buffer rayBuffer;
	vector<cl::Event> eventList;
	vector<RayStruct> rays = vector<RayStruct>();
#else
	vector<Ray> rays;
#endif
	const shared_ptr<const vector<Triangle>> triangles;
	const shared_ptr<BoundingVolume> boundingVolume;
public:
	//add a ray to the buffer, return its index
	int enqueueRay(Ray ray);

	//fire off the current buffer of rays
	bool castRays(bool sync = true);

	//get the index of the triangle this ray collided with
	int getRayCollision(int index);

	bool flushBuffer();

	Triangle getRayTriangleCollision(int index);

	Ray getRay(int index, bool &anyCollision);

	RayCaster(const shared_ptr<const vector<Triangle>> triangles, const shared_ptr<BoundingVolume> boundingVolume, bool shittyDrivers = true);


};