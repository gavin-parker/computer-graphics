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
inline float3 norm(float3 vec){
	float len = sqrt(vec.x*vec.x + vec.y*vec.y + vec.z*vec.z);
	return (float3){vec.x/len, vec.y/len, vec.z/len };
}
inline float len(float3 vec){ 
	return sqrt(vec.x*vec.x + vec.y*vec.y + vec.z*vec.z);
}
inline float3 phong(float3 v, float3 l, float3 n){
	float3 r = l - 2.f* dot((dot(n,l)),n);
	float3 spec = dot(v,r);
	float power = len(spec);
	return (float3){power,power,power};
}

inline float3 directLight(Ray ray, float3 lightPos, float3 normal){
	float3 offset = lightPos - ray.collisionLocation;
	float3 direction = norm(offset);
	float radius = len(offset);
	float3 color = {0.75f, 0.75f, 0.75f };
	return (max(dot(direction, normal), 0.0f) * 500000.f / (4.0f * M_PI * radius * radius)) * color;
}



kernel void getPixel(global const TriangleStruct* triangles,float3 lightLoc, global float3* image, global Camera* camera, int triangleCount, int width, int height){
	int x = get_global_id(0);
	int y = get_global_id(1);
	Ray cameraRay;
	cameraRay.origin = camera->position;	

	float3 cameraSpaceDirection = (float3){lerpF(camera->viewOffset, -camera->viewOffset, (float)x/(float)width), lerpF(camera->viewOffset, -camera->viewOffset, (float)y/(float)height), 1.0f};

	camera->rotation[0] = (float3){1.f, 0.f, 0.f };
	camera->rotation[1] = (float3){0.f, 1.f, 0.f };
	camera->rotation[2] = (float3){0.f, 0.f, 1.f };


	//dot product for camera ray
	cameraRay.direction.x = dot(camera->rotation[0], cameraSpaceDirection);
	cameraRay.direction.y = dot(camera->rotation[1], cameraSpaceDirection);
	cameraRay.direction.z = dot(camera->rotation[2], cameraSpaceDirection);


	cameraRay.length = FLT_MAX;

	//calculate intersections for camera ray
	bool anyIntersection = false;
	for(int i=0; i < triangleCount; i++){
		if(dot(cameraRay.direction, triangles[i].normal) < 0){

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
	  	float3 lightColour = {0.f,0.f,0.f };
			Ray lightRay;
			lightRay.origin = lightLoc;
			lightRay.direction = cameraRay.collisionLocation - lightLoc;


				//calculate intersections for light ray
			bool lightIntersection = false;
			for(int i=0; i < triangleCount; i++){
				if(dot(lightRay.direction, triangles[i].normal) < 0){

					float3 b = lightRay.origin - triangles[i].v0;
					float3 e1 = triangles[i].v1 - triangles[i].v0;
					float3 e2 = triangles[i].v2 - triangles[i].v0;

					float3 A[3] = {-lightRay.direction, e1, e2 };
					float det_A = det(A);
					float3 B[3] = {b, e1, e2 };
					float t = det(B) / det_A;
					if (t >= 0 && t < lightRay.length) {
						float3 U[3] = {-lightRay.direction, b, e2};
						float3 V[3] = { -lightRay.direction, e1, b};
						float u = det(U) / det_A;
						float v = det(V) / det_A;

						if (u >= 0 && v >= 0 && (u + v) < 1) {
							lightRay.length = t;
							lightRay.collision = i;
							lightRay.collisionLocation = triangles[i].v0 + u * e1 + v * e2;
							//cameraRay.collisionUVLocation = vt0 + u * et1 + v * et2;
							lightIntersection = true;

						}
					}
				}
			}

			if(lightIntersection && lightRay.collision == cameraRay.collision){
				float3 n = triangles[lightRay.collision].normal;
				float3 v = norm(cameraRay.direction);
				float3 l = norm(lightRay.direction);
				float3 spec = phong(v,l,n);
				float diffuse = 0.75f;
				float specularity = 0.4f;
				lightColour = directLight(lightRay, lightLoc, n)*diffuse + spec * specularity;
				lightColour *= triangles[lightRay.collision].color;
			 }else{
				float3 ambient = {0.3f, 0.3f, 0.3f };
				lightColour = triangles[cameraRay.collision].color*ambient;
			  }
			image[(y*width + x)] = lightColour;
		}else{
			image[(y*width + x)] = (float3){0.f, 0.f, 0.f };
		}

	return;
 }

