//#define M_PI 3.14159265359f
typedef struct TriangleStruct {
	float3 v0;
	float3 v1;
	float3 v2;
	float3 color;
	float3 normal;
} TriangleStruct;

typedef struct Ray {
	float3 origin;
	float3 direction;
	float3 collisionLocation;
	float length;
	int collision;
} Ray;

inline float lerpF(float a, float b, float t) { return a + (b - a) * t; }

inline float dot_product(float3 a, float3 b) {
	return a.x*b.x + a.y*b.y + a.y*b.y;
}

inline float det(float3 mat[3]) {
	return mat[0].x*(mat[1].y*mat[2].z - mat[1].z*mat[2].y) - mat[0].y*(mat[1].x*mat[2].z - mat[1].z*mat[2].x) + mat[0].z*(mat[1].x*mat[2].y - mat[1].y*mat[2].x);
}
inline float3 norm(float3 vec) {
	float len = native_rsqrt(vec.x*vec.x + vec.y*vec.y + vec.z*vec.z);
	return (float3) { vec.x*len, vec.y*len, vec.z*len };
}
inline float len(float3 vec) {
	return native_sqrt(vec.x*vec.x + vec.y*vec.y + vec.z*vec.z);
}
inline float3 phong(float3 v, float3 l, float3 n) {
	float3 r = l - 2.f* dot((dot(n, l)), n);
	float3 spec = dot(v, r);
	float power = len(spec);
	return (float3) { power, power, power };
}

inline float3 directLight(Ray ray, float3 lightPos, float3 normal) {
	float3 offset = lightPos - ray.collisionLocation;
	float3 direction = norm(offset);
	float radius = len(offset);
	float3 color = { 0.75f, 0.75f, 0.75f };
	return (max(dot(direction, normal), 0.0f) * 500000.f / (4.0f * (float)M_PI * radius * radius)) * color;
}

kernel void pathTrace(global const TriangleStruct* triangles, float3 lightLoc, global float3* image, global float3* points) {


}


kernel void flatShade(global const TriangleStruct* triangles, float3 lightLoc, global Ray* points, global float3* image, int triangleCount, int width, int height) {
	int x = get_global_id(0);
	int y = get_global_id(1);
	Ray ray = points[(y*width + x)];
	float3 color = (float3) { 0, 0, 0 };
	if (ray.collision > -1 && ray.collision < triangleCount) {
		color = triangles[ray.collision].color;
	}
	image[(y*width + x)] = color;
}

kernel void standardShade(global const TriangleStruct* triangles, float3 lightLoc, global Ray* points, global float3* image, int triangleCount, int width, int height) {
	int x = get_global_id(0);
	int y = get_global_id(1);

	Ray cameraRay = points[(y*width + x)];
	float3 color = (float3) ( 0, 0, 0 );
	if (cameraRay.collision > -1) {
		Ray lightRay;
		lightRay.origin = lightLoc;
		lightRay.direction = cameraRay.collisionLocation - lightLoc;
		lightRay.collision = -1;
		bool lightIntersection = false;
		//NOT FINDING ANY INTERESECTIONS :(
		for (int i = 0; i < triangleCount; i++) {
			if (dot(lightRay.direction, triangles[i].normal) < 0) {

				float3 b = lightRay.origin - triangles[i].v0;
				float3 e1 = triangles[i].v1 - triangles[i].v0;
				float3 e2 = triangles[i].v2 - triangles[i].v0;

				float3 A[3] = { -lightRay.direction, e1, e2 };
				float det_A = native_recip(det(A));
				float3 B[3] = { b, e1, e2 };
				float t = det(B) * det_A;
				if (t >= 0 && t < lightRay.length) {
					float3 U[3] = { -lightRay.direction, b, e2 };
					float3 V[3] = { -lightRay.direction, e1, b };
					float u = det(U) * det_A;
					float v = det(V) * det_A;

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
		if (lightRay.collision == cameraRay.collision) {
			float3 n = triangles[lightRay.collision].normal;
			float3 v = norm(cameraRay.direction);
			float3 l = norm(lightRay.direction);
			float3 spec = phong(v, l, n);
			float diffuse = 0.75f;
			float specularity = 0.4f;
			printf("direct light\n");
			//lightColour = directLight(lightRay, lightLoc, n)*diffuse + spec * specularity;
			color = triangles[lightRay.collision].color* diffuse;
		}else {
			printf("ambient light\n");
			float3 ambient = 0.1f;
			color = ambient*triangles[cameraRay.collision].color*0.75f;
		}
		//color = (float3) { min(lightColour.x, 1.0f), min(lightColour.y, 1.0f), min(lightColour.z, 1.0f) };
	}
	image[(y*width + x)] = color;
}

kernel void castRays(global const TriangleStruct* triangles, float3 lightLoc, global Ray* points, global float* camera, int triangleCount, int width, int height) {

	int x = get_global_id(0);
	int y = get_global_id(1);

	float3 cameraPos = (float3){(float)camera[0],(float)camera[1],(float)camera[2] };
	Ray cameraRay;
	cameraRay.origin = cameraPos;

	float3 cameraSpaceDirection = (float3) { lerpF(camera[3], -camera[3], (float)x / (float)width), lerpF(camera[3], -camera[3], (float)y / (float)height), 1.0f };

	//float3 rotation[3];
	//rotation[0] = (float3) { 1.f, 0.f, 0.f };
	//rotation[1] = (float3) { 0.f, 1.f, 0.f };
	//rotation[2] = (float3) { 0.f, 0.f, 1.f };


	float3 rotation[3];
	rotation[0] = (float3) { camera[4], camera[5], camera[6] };
	rotation[1] = (float3) { camera[7], camera[8], camera[9] };
	rotation[2] = (float3) { camera[10], camera[11], camera[12]  };
	//dot product for camera ray
	cameraRay.direction.x = dot(rotation[0], cameraSpaceDirection);
	cameraRay.direction.y = dot(rotation[1], cameraSpaceDirection);
	cameraRay.direction.z = dot(rotation[2], cameraSpaceDirection);

	cameraRay.length = FLT_MAX;
	cameraRay.collision = -1;
	//calculate intersections for camera ray
	//bool anyIntersection = false;
#pragma unroll
	for (int i = 0; i < triangleCount; i++) {
		if (dot(cameraRay.direction, triangles[i].normal) < 0) {

			float3 b = cameraRay.origin - triangles[i].v0;
			float3 e1 = triangles[i].v1 - triangles[i].v0;
			float3 e2 = triangles[i].v2 - triangles[i].v0;

			float3 A[3] = { -cameraRay.direction, e1, e2 };
			float det_A = native_recip(det(A));
			float3 B[3] = { b, e1, e2 };
			float t = det(B) * det_A;
			if (t >= 0 && t < cameraRay.length) {
				float3 U[3] = { -cameraRay.direction, b, e2 };
				float3 V[3] = { -cameraRay.direction, e1, b };
				float u = det(U) * det_A;
				float v = det(V) * det_A;

				if (u >= 0 && v >= 0 && (u + v) < 1) {
					cameraRay.length = t;
					cameraRay.collision = i;
					cameraRay.collisionLocation = triangles[i].v0 + u * e1 + v * e2;
					//cameraRay.collisionUVLocation = vt0 + u * et1 + v * et2;
					//anyIntersection = true;

				}
			}
		}
	}
	points[(y*width + x)] = cameraRay;
	points[(y*width + x)].collisionLocation = cameraRay.collisionLocation;
	return;

}

