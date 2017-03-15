#pragma once
#include "ray.h"
#include "bvh.h"
#ifdef useCL
#include "clwrapper.h"
#endif
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


class RayCaster {
private:
	int maxRays = 26000;
	int chunkIndex = 0;
	int lastSize = 0;
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
	bool castRays(bool sync = true, int bottom = 0, int size = -1);

	//get the index of the triangle this ray collided with
	int getRayCollision(int index);

	bool flushBuffer();

	Triangle getRayTriangleCollision(int index);

	Ray getRay(int index, bool &anyCollision);

	RayCaster(const shared_ptr<const vector<Triangle>> triangles, const shared_ptr<BoundingVolume> boundingVolume, bool shittyDrivers = true);


};