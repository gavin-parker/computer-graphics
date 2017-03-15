#include "standardlighting.h"

// GlobalIllumination::GlobalIllumination(){};

StandardLighting::StandardLighting(const shared_ptr<Scene> scene) : LightingEngine(scene->triangles, scene->light), boundingVolume(scene->volume){};

vec3 StandardLighting::calculateLight(Ray ray, ivec2 pixel) {
  Ray lightRay;
  vector<Ray> rays(light->rayCount);
   
  light->calculateRays(rays, ray.collisionLocation);
  // ClosestIntersection(lightRay);

  shared_ptr<const Material> mat = ray.collision->mat;

  vec3 lightColour(0, 0, 0);

  //calculate average light at a point -- works with multiple light rays
  for (int i = 0; (size_t)i < rays.size(); i++) {
	  lightRay = rays[i];

	  if (boundingVolume->calculateAnyIntersection(lightRay, ray) && sametriangle(lightRay.collision, ray.collision)) {

		  vec3 n = lightRay.collision->normal;
		  vec3 v = glm::normalize(ray.direction);
		  vec3 l = glm::normalize(lightRay.direction);
		  vec3 spec = mat->phong(v, l, n);

		  lightColour +=
			  light->directLight(ray) * mat->diffuse + spec * mat->specularity;
	  }
	  else {
		  lightColour += ambientLight * mat->diffuse;
	  }
  }
  lightColour /= rays.size();
  return ray.collision->getPixelColour(ray.collisionUVLocation) * lightColour;
}

bool StandardLighting::ClosestIntersection(Ray &ray) {
  ray.length = numeric_limits<float>::max();

  bool anyIntersection = false;

  for (const Triangle &triangle : *triangles) {
    anyIntersection |= triangle.calculateIntersection(ray);
  }

  return anyIntersection;
}

// like closestIntersection, but backs out after a single intersection
bool StandardLighting::anyIntersection(Ray &ray, Ray &surface) {
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
