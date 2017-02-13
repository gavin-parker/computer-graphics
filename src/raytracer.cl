struct TriangleStruct {
	float* v0[3];
	float* v1[3];
	float* v2[3];
	float* color[3];
};

kernel void getPixel(global const struct TriangleStruct* triangles, global const float* lightLoc, global float* image, int width, int height){
	int jj = get_global_id(0);
	int ii = get_global_id(1);
	image[(ii*width + jj)*3] = 0.f;
	image[(ii*width + jj)*3+1] = 1.f;
	image[(ii*width + jj)*3+2] = 0.f;
	return;
 }