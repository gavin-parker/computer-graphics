#include "gi.h"

// GlobalIllumination::GlobalIllumination(){};

GlobalIllumination::GlobalIllumination(Scene scene, int sampleCount) : LightingEngine(scene), sampleCount(sampleCount), boundingBox(new Cube(triangles)){};

vec3 GlobalIllumination::trace(Ray ray, int bounces) {
  // find diffuse light at this position
  float diffuse = ray.collision->mat->diffuse;
  vec3 lightHere(0, 0, 0);
  Ray directLightRay;
  light->calculateRay(directLightRay, ray.collisionLocation);

  if (!anyIntersection(directLightRay, ray)) {
    lightHere = vec3(0, 0, 0);
  } else if (directLightRay.collision == ray.collision) {
    lightHere = light->directLight(ray)*ray.collision->getPixelColour(ray.collisionUVLocation);
  }

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
      if (ClosestIntersection(bounce)) {
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

bool GlobalIllumination::ClosestIntersection(Ray &ray) {
  ray.length = numeric_limits<float>::max();

  bool anyIntersection = false;
  if (!boundingBox->calculateIntersection(ray)) {
	  return false;
  }

  for (const Triangle &triangle : *triangles) {
    anyIntersection |= triangle.calculateIntersection(ray);
  }

  return anyIntersection;
}

// like closestIntersection, but backs out after a single intersection
bool GlobalIllumination::anyIntersection(Ray &ray, Ray &surface) {
  bool anyIntersection = false;
  float lightDistance = ray.length;
  ray.length = numeric_limits<float>::max();
  for (const Triangle &triangle : *triangles) {
    anyIntersection |= triangle.calculateIntersection(ray);
    if (anyIntersection && ray.collision != surface.collision &&
        ray.length < lightDistance) {
      return anyIntersection;
    }
  }
  return anyIntersection;
}
