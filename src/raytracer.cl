

struct TriangleStruct {
	float* v0[3];
	float* v1[3];
	float* v2[3];
	float* color[3];
};

void kernel getPixel(global const struct TriangleStruct* triangles, global const float* lightLoc){
	printf("loaded scene with triangles");
	return;
 };