#pragma once
#include <limits>
#include <omp.h>
#include "camera.h"
#include "convergent_gi.h"
#include "cube.h"
#include "light.h"
#include "sdlscreen.h"
#include "testmodel.h"
#include "bvh.h"
#include "raycaster.h"

using std::numeric_limits;

class WaveFrontRenderer : public SdlScreen {
private:
	//bool ClosestIntersection(Ray &ray);
	const shared_ptr<const vector<Triangle>> triangles;
	Camera camera;
	const vec3 ambientLight = vec3(0.1f, 0.1f, 0.1f);
	const shared_ptr<Light> light;
	shared_ptr<LightingEngine> lighting;
	const shared_ptr<BoundingVolume> boundingVolume;
	const shared_ptr<RayCaster> rayCaster;
	bool antialias;
	int chunkSize = 4000;
	vector<int> rayIndices;
	vector<Ray> rays;
protected:
	void update(float dt) override;
	void draw(int width, int height) override;
	void computeIndirectLight(int chunkStart, int chunkSize, int samples, int bounces);
public:
	WaveFrontRenderer(int width, int height, shared_ptr<LightingEngine> lighting,
		const shared_ptr<Light> light,
		const shared_ptr<const vector<Triangle>> triangles, const shared_ptr<BoundingVolume> boundingVolume,
		bool fullscreen = false, bool antialias = true);
};
