#include "gi.h"

// GlobalIllumination::GlobalIllumination(){};

GlobalIllumination::GlobalIllumination(shared_ptr<Scene> scene, int sampleCount) : LightingEngine(scene->triangles, scene->light), sampleCount(sampleCount), boundingVolume(scene->volume){};

vec3 GlobalIllumination::trace(Ray ray, int bounces) {
  // find diffuse light at this position
  float diffuse = ray.collision->mat->diffuse;
  vec3 lightHere(0, 0, 0);

  vector<Ray> rays;
  light->calculateRays(rays, ray.collisionLocation);

  for (int i = 0; i < rays.size(); i++) {
	  Ray directLightRay = rays[i];

	  if (!boundingVolume->calculateAnyIntersection(directLightRay, ray)) {
		  lightHere += vec3(0, 0, 0);
	  }
	  else if (directLightRay.collision == ray.collision) {
		  lightHere += light->directLight(ray)*ray.collision->getPixelColour(ray.collisionUVLocation);
	  }
  }
  lightHere /= rays.size();
  vec3 indirectLight(0, 0, 0);

  // create orthogonal basis on plane
  vec3 normal = ray.collision->normal;
  vec3 normalX;
  vec3 normalY;

  if (abs(normal.x) > abs(normal.y)) {
	  normalX = vec3(normal.z, 0, -normal.x) / sqrtf(normal.x*normal.x + normal.z*normal.z);
  }
  else {
	  normalX = vec3(0, -normal.z, normal.y) / sqrtf(normal.z*normal.z + normal.y*normal.y);
  }
  normalY = glm::cross(normalX, normal);


  mat3 basis(normalX, normalY, normal);
  for (int i = 0; i < sampleCount; i++) {

    // generate random direction
	  float r1 = RAND;
	  float r2 = RAND;
	  float sinTheta = sqrtf(1 - r1*r1);
	  float phi = 2 * M_PI * r2;
	  float x = sinTheta * cosf(phi);
	  float z = sinTheta * sinf(phi);
	  vec3 sample(x, r1, z);

	  vec3 direction(sample.x * normalX.x + sample.y * normal.x + sample.z * normalY.x,
		  sample.x * normalX.y + sample.y * normal.y + sample.z * normalY.y,
		  sample.x * normalX.z + sample.y * normal.z + sample.z * normalY.z);

    if (bounces >= 1) {
      Ray bounce;
      bounce.position = ray.collisionLocation;
      bounce.direction = glm::normalize(direction);
      // return this + new collision point
      if (boundingVolume->calculateIntersection(bounce)) {
        indirectLight += r1 * trace(bounce, bounces - 1);
	  }
	  else {
		  indirectLight += r1 * vec3(1, 1, 1)*environment; // assume white environment sphere
	  }
    } else {
      return (lightHere / static_cast<float>(M_PI))*diffuse;
    }
  }
  indirectLight /= static_cast<float>(sampleCount);
  return (lightHere / static_cast<float>(M_PI) + 2.f * indirectLight)*diffuse;
}

vec3 GlobalIllumination::calculateLight(Ray ray, ivec2 pixel) {
  return trace(ray, total_bounces)* ray.collision->getPixelColour(ray.collisionUVLocation);
}


