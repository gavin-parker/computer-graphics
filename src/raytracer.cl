//#define M_PI 3.14159265359f
#define sampleCount 3
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

typedef struct Basis{
	float3 x;
	float3 y;
	float3 z;
 } Basis;

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
inline float randomNumberGenerator(float seed){
	return fmod(seed*10,1);
}
inline Ray castRayLocal(float3 origin, float3 direction, local TriangleStruct* triangles, int triangleCount){
	Ray ray;
	ray.origin = origin;
	ray.direction = direction;
	ray.length = FLT_MAX;
	ray.collision = -1;
	ray.collisionLocation = (float3){0,0,0};
	#pragma unroll
	for (int i = 0; i < triangleCount; i++) {
		int index = (dot(ray.direction, triangles[i].normal) < 0) ? 1 : -1;
		float3 b = origin - triangles[i].v0;
		float3 e1 = triangles[i].v1 - triangles[i].v0;
		float3 e2 = triangles[i].v2 - triangles[i].v0;

		float3 A[3] = { -direction, e1, e2 };
		float det_A = native_recip(det(A));
		float3 B[3] = { b, e1, e2 };
		float t = det(B) * det_A;

		index *= (t >= 0 && t < ray.length) ? 1 : -1;
		float3 U[3] = { -ray.direction, b, e2 };
		float3 V[3] = { -ray.direction, e1, b };
		float u = det(U) * det_A;
		float v = det(V) * det_A;

		index *= (u >= 0 && v >= 0 && (u + v) < 1) ? 1 : -1;

		ray.length = (index > 0) ? t : ray.length;
		ray.collision = (index > 0) ? i : ray.collision;
		ray.collisionLocation = triangles[i].v0 + u * e1 + v * e2;
	}
		return ray;
}

inline Ray castRay(float3 origin, float3 direction, global const TriangleStruct* triangles, int triangleCount){
	Ray ray;
	ray.origin = origin;
	ray.direction = direction;
	ray.length = FLT_MAX;
	ray.collision = -1;

	for (int i = 0; i < triangleCount; i++) {
		if (dot(ray.direction, triangles[i].normal) < 0) {
			float3 b = origin - triangles[i].v0;
			float3 e1 = triangles[i].v1 - triangles[i].v0;
			float3 e2 = triangles[i].v2 - triangles[i].v0;

			float3 A[3] = { -direction, e1, e2 };
			float det_A = native_recip(det(A));
			float3 B[3] = { b, e1, e2 };
			float t = det(B) * det_A;
			if (t >= 0 && t < ray.length) {
				float3 U[3] = { -ray.direction, b, e2 };
				float3 V[3] = { -ray.direction, e1, b };
				float u = det(U) * det_A;
				float v = det(V) * det_A;

				if (u >= 0 && v >= 0 && (u + v) < 1) {
					ray.length = t;
					ray.collision = i;
					ray.collisionLocation = triangles[i].v0 + u * e1 + v * e2;
				}
			}
		}
	}
		return ray;
}


 inline float3 randomDirection(float r1, float r2, float3 normal){
	float3 normalX;
	float3 normalY;
	float mask = (fabs(normal.x) > fabs(normal.y)) ? 1 : 0;
	normalX = ((float3){normal.z, 0, -normal.x} / native_sqrt(normal.x*normal.x + normal.z*normal.z))*mask;
	mask = (mask > 0) ? 0 : 1;
	normalX += ((float3){0, -normal.z, normal.y} / native_sqrt(normal.z*normal.z + normal.y*normal.y))*mask;
	normalY = cross(normalX, normal);
	float sinTheta = native_sqrt(1 - r1*r1);
	float phi = 2 * M_PI * r2;
	float x = sinTheta * native_cos(phi);
	float z = sinTheta * native_sin(phi);
	float3 sample = (float3){x, r1, z};

	float3 direction = (float3){sample.x * normalX.x + sample.y * normal.x + sample.z * normalY.x,
		sample.x * normalX.y + sample.y * normal.y + sample.z * normalY.y,
		sample.x * normalX.z + sample.y * normal.z + sample.z * normalY.z};
	return direction;
  }

kernel void pathTrace(global const TriangleStruct* triangles_global,local TriangleStruct* triangles, float3 lightLoc, global Ray* points, global float3* image, int triangleCount, int width, int height, global float* rands) {
	int x = get_global_id(0);
	int y = get_global_id(1);

	#pragma unroll
	for(int i=0; i < triangleCount; i++){
		triangles[i] = triangles_global[i];
	}
    barrier(CLK_GLOBAL_MEM_FENCE);
	Ray cameraRay = points[(y*width + x)];
	TriangleStruct triangle = triangles[cameraRay.collision];
	float3 color = (float3) ( 0, 0, 0 );

	//DIRECT LIGHT
	float maskA = (-1 < cameraRay.collision) ? 1.f : 0.f;

	Ray lightRay = castRayLocal(lightLoc, cameraRay.collisionLocation - lightLoc, triangles, triangleCount);
	
	int diff = cameraRay.collision - lightRay.collision;
	float maskB = (diff == 0) ? 	1.f : 0.f;

	float3 n = triangle.normal;
	float3 v = norm(cameraRay.direction);
	float3 l = norm(lightRay.direction);
	float3 spec = phong(v, l, n);
	float diffuse = 0.75f;
	float specularity = 0.1f;
	//lightColour = directLight(lightRay, lightLoc, n)*diffuse + spec * specularity;
	color = directLight(lightRay, lightLoc, n)*diffuse + spec * specularity*maskA*maskB;
	color *= triangle.color;
	

	//generates and calculates all the ray intersections needed
	Ray layer1[sampleCount];
	float r1 = rands[(y*width + x)];
	#pragma unroll
	for(int i=0; i < sampleCount; i++){
		float r2 = randomNumberGenerator(r1);
		float3 direction = norm(randomDirection(r1, r2, triangle.normal));
		layer1[i] = castRayLocal(cameraRay.collisionLocation, direction, triangles, triangleCount);
		r1 = randomNumberGenerator(r2);
	}
	r1 = rands[((height-y)*width + x)];
	Ray layer2[(sampleCount*sampleCount)];
	#pragma unroll
	for(int i=0; i < sampleCount; i++){
		triangle = triangles[layer1[i].collision];
		#pragma unroll
		for(int j=0; j < sampleCount; j++){
			float r2 = randomNumberGenerator(r1);
			float3 direction = norm(randomDirection(r1, r2, triangle.normal));
			layer1[i*sampleCount+j] = castRayLocal(layer1[i].collisionLocation, direction, triangles, triangleCount);
			r1 = randomNumberGenerator(r2);
		}
	}
	r1 = rands[y*width + (width-x)];

	#pragma unroll
	for(int i=0; i < (sampleCount*sampleCount); i++){
		Ray lightRay = castRayLocal(lightLoc, layer2[i].collisionLocation - lightLoc, triangles, triangleCount);
		float3 directLightHere;
        triangle = triangles[layer2[i].collision];
        float3 n = triangle.normal;
        directLightHere = directLight(lightRay, lightLoc, n)*0.75f;
        directLightHere *= triangle.color;
        directLightHere = (lightRay.collision == layer2[i].collision && lightRay.collision > -1) ? directLightHere : (float3){0.2,0.2,0.2};


		layer2[i].origin = (directLightHere/(float)M_PI)*0.75f;
	}
	#pragma unroll
	for(int i=0; i < sampleCount; i++){
		Ray lightRay = castRayLocal(lightLoc, layer1[i].collisionLocation - lightLoc, triangles, triangleCount);
		float3 directLightHere;
        triangle = triangles[layer1[i].collision];
        float3 n = triangle.normal;
        directLightHere = directLight(lightRay, lightLoc, n)*0.75f;
        directLightHere *= triangle.color;
    
        directLightHere = (lightRay.collision == layer1[i].collision && lightRay.collision > -1) ? directLightHere : (float3){0.2,0.2,0.2};

		float3 indirectLight = (float3){0,0,0};
        #pragma unroll
		for(int j=0; j < sampleCount; j++){
			indirectLight += r1*layer2[i*sampleCount+j].origin;
		}
		indirectLight /= sampleCount;
		layer1[i].origin = (directLightHere / (float)M_PI + 2.f * indirectLight)*0.75f;
	}

	float3 indirectLight = (float3){0,0,0};
	#pragma unroll
	for(int j=0; j < sampleCount; j++){
		indirectLight += layer1[j].origin;
	}
	indirectLight /=sampleCount;
	float3 finalLight = (color / (float)M_PI + 2.f * indirectLight)*0.75f;

	image[(y*width + x)] = (float3) { min(finalLight.x, 1.0f), min(finalLight.y, 1.0f), min(finalLight.z, 1.0f) };

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
	TriangleStruct triangle = triangles[cameraRay.collision];
	float3 color = (float3) ( 0, 0, 0 );
	if (cameraRay.collision > -1) {
		Ray lightRay;
		lightRay.origin = lightLoc;
		lightRay.direction = cameraRay.collisionLocation - lightLoc;
		lightRay.collision = -1;
		lightRay.length = FLT_MAX;
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

					}
				}
			}
		}
		if (cameraRay.collision == lightRay.collision) {
			float3 n = triangle.normal;
			float3 v = norm(cameraRay.direction);
			float3 l = norm(lightRay.direction);
			float3 spec = phong(v, l, n);
			float diffuse = 0.75f;
			float specularity = 0.1f;
			//lightColour = directLight(lightRay, lightLoc, n)*diffuse + spec * specularity;
			color = directLight(lightRay, lightLoc, n)*diffuse + spec * specularity;
			color *= triangle.color;
		}else {
			float3 ambient = 0.1f;
			color = ambient*triangle.color;
		}
		//color = (float3) { min(lightColour.x, 1.0f), min(lightColour.y, 1.0f), min(lightColour.z, 1.0f) };
	}
    barrier(CLK_GLOBAL_MEM_FENCE);
	image[(y*width + x)] = (float3) { min(color.x, 1.0f), min(color.y, 1.0f), min(color.z, 1.0f) };
}

kernel void castRays(global const TriangleStruct* triangles, float3 lightLoc, global Ray* points, global float* camera, int triangleCount, int width, int height) {

	int x = get_global_id(0);
	int y = get_global_id(1);

	float3 cameraPos = (float3){(float)camera[0],(float)camera[1],(float)camera[2] };
	Ray cameraRay;
	cameraRay.origin = cameraPos;

	float3 cameraSpaceDirection = (float3) { lerpF(camera[3], -camera[3], (float)x / (float)width), lerpF(camera[3], -camera[3], (float)y / (float)height), 1.0f };
	float3 rotation[3];
	rotation[0] = (float3) { camera[4], camera[5], camera[6] };
	rotation[1] = (float3) { camera[7], camera[8], camera[9] };
	rotation[2] = (float3) { camera[10], camera[11], camera[12]  };
	//dot product for camera ray
	cameraRay.direction.x = dot(rotation[0], cameraSpaceDirection);
	cameraRay.direction.y = dot(rotation[1], cameraSpaceDirection);
	cameraRay.direction.z = dot(rotation[2], cameraSpaceDirection);
	points[(y*width + x)] = castRay(cameraRay.origin, cameraRay.direction, triangles, triangleCount);;
	return;

}

