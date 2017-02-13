#define M_PI 3.14159265359f
#pragma OPENCL EXTENSION cl_intel_printf : enable

typedef struct TriangleStruct {
	float3 v0;
	float3 v1;
	float3 v2;
	float3 color;
	float3 normal;
} TriangleStruct;

typedef struct Ray{
	float3 origin;
	float3 direction;
	int collision;
	float length;
	float3 collisionLocation;
 } Ray;

typedef struct Camera{ 
	float3 position;
	float viewOffset;
	float3 rotation[3];
} Camera;

inline float lerpF(float a, float b, float t) { return a + (b - a) * t; }


inline float dot_product(float3 a, float3 b){
	return a.x*b.x + a.y*b.y+a.y*b.y;
 }

 inline float det(float3 mat[3]){
	return mat[0].x*(mat[1].y*mat[2].z - mat[1].z*mat[2].y) - mat[0].y*(mat[1].x*mat[2].z - mat[1].z*mat[2].x) + mat[0].z*(mat[1].x*mat[2].y - mat[1].y*mat[2].x);
 }


kernel void getPixel(global const TriangleStruct* triangles, global const float3* lightLoc, global float3* image, int triangleCount, int width, int height){
	int x = get_global_id(0);
	int y = get_global_id(1);
	Camera camera;
	camera.position = (float3){ 277.5f, 277.5f, -480.64};
	camera.viewOffset = (float)(tanpi(30.f / 180.f));
	Ray cameraRay;
	cameraRay.origin = camera.position;
	float3 cameraSpaceDirection = (float3){lerpF(camera.viewOffset, -camera.viewOffset, (float)x/(float)width), lerpF(camera.viewOffset, -camera.viewOffset, (float)y/(float)height), 1.0f};

	//dot product for camera ray
	cameraRay.direction.x = dot(camera.rotation[0], cameraSpaceDirection);
	cameraRay.direction.y = dot(camera.rotation[1], cameraSpaceDirection);
	cameraRay.direction.z = dot(camera.rotation[2], cameraSpaceDirection);





	cameraRay.length = FLT_MAX;


	//calculate intersections
	bool anyIntersection = false;
	for(int i=0; i < triangleCount; i++){
		if(dot(cameraRay.direction, triangles[i].normal) < 0){
			 //vec3 b = ray.position - v0;
			float3 b = cameraRay.origin - triangles[i].v0;
			float3 e1 = triangles[i].v1 - triangles[i].v0;
			float3 e2 = triangles[i].v2 - triangles[i].v0;

			float3 A[3] = {-cameraRay.direction, e1, e2 };
			float det_A = det(A);
			float3 B[3] = {b, e1, e2 };
			float t = det(B) / det_A;
			if (t >= 0 && t < cameraRay.length) {
				float3 U[3] = {-cameraRay.direction, b, e2};
				float3 V[3] = { -cameraRay.direction, e1, b};
				float u = det(U) / det_A;
				float v = det(V) / det_A;
				if (u >= 0 && v >= 0 && (u + v) < 1) {
						cameraRay.length = t;
						cameraRay.collision = i;
						cameraRay.collisionLocation = triangles[i].v0 + u * e1 + v * e2;
						//cameraRay.collisionUVLocation = vt0 + u * et1 + v * et2;
						anyIntersection = true;
				}
			}
		}
	}


      if (anyIntersection) {
			float3 lightColour = triangles[cameraRay.collision].color;
			image[(y*width + x)] = lightColour;
		}else{
			image[(y*width + x)] = (float3){0.f,0.f,0.f};
		}

	return;
 }

