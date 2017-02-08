#include "raytracer.h"

RayTracer::RayTracer(int width, int height, bool fullscreen)
    : SdlScreen(width, height, fullscreen), triangles(loadTestModel()),
      camera(vec3(277.5f, 277.5f, -480.64), 0.0f, 30.0f),
      light(vec3(400.0f, 100.0f, 100.0f), vec3(1.0, 1.0f, 1.0f), 500000.0f) {}

void RayTracer::update(float dt) {
  light.update(dt);
  camera.update(dt);
}

void RayTracer::draw(int width, int height) {
#pragma omp parallel for
  for (int y = 0; y < height; ++y) {
    for (int x = 0; x < width; ++x) {
      Ray cameraRay, lightRay;

      camera.calculateRay(cameraRay, static_cast<float>(x) / width,
                          static_cast<float>(y) / height);

      if (ClosestIntersection(cameraRay)) {
        light.calculateRay(lightRay, cameraRay.collisionLocation);
        ClosestIntersection(lightRay);

        vec3 lightColour(0, 0, 0);
        shared_ptr<const Material> mat = cameraRay.collision->mat;
        if (lightRay.collision == cameraRay.collision) {

          vec3 n = lightRay.collision->normal;
          vec3 v = glm::normalize(cameraRay.direction);
          vec3 l = glm::normalize(lightRay.direction);
          vec3 spec = mat->phong(v, l, n);

          lightColour = globalIllumination(cameraRay, 3) * mat->diffuse +
                        spec * mat->specularity;

        } else {
          lightColour = globalIllumination(cameraRay, 3) * mat->diffuse;
        }

        vec3 surfaceColour =
            cameraRay.collision->getPixelColour(cameraRay.collisionUVLocation);

        drawPixel(x, y, vec3(std::min(surfaceColour.r * lightColour.r, 1.0f),
                             std::min(surfaceColour.g * lightColour.g, 1.0f),
                             std::min(surfaceColour.b * lightColour.b, 1.0f)));
      }
    }
  }
}

bool RayTracer::ClosestIntersection(Ray &ray) {
  ray.length = numeric_limits<float>::max();

  bool anyIntersection = false;

  for (const Triangle &triangle : *triangles) {
    anyIntersection |= triangle.calculateIntection(ray);
  }

  return anyIntersection;
}

vec3 RayTracer::globalIllumination(Ray ray, int bounces) {

  // find diffuse light at this position
  if (bounces > 0) {
    float diffuse = ray.collision->mat->diffuse;
    vec3 surfaceColour = ray.collision->getPixelColour(ray.collisionUVLocation);
    vec3 lightHere = ray.collision->getPixelColour(ray.collisionUVLocation);
    Ray directLightRay;
    light.calculateRay(directLightRay, ray.collisionLocation);
    if (!ClosestIntersection(directLightRay)) {
      return vec3(0, 0, 0);
    }
    if (directLightRay.collision == ray.collision) {
      lightHere *= light.directLight(ray) * diffuse;
    }

    vec3 normal = ray.collision->normal;
    vec3 indirectLight(0, 0, 0);

    // create orthogonal basis on plane
    vec3 nx, nb;
    if (normal.x > normal.y) {
      nx = vec3(normal.z, 0, -normal.x) /
           static_cast<float>(sqrt(normal.x * normal.x + normal.z * normal.z));
      nb = glm::cross(nx, normal);
    } else {
      nx = vec3(0, -normal.z, normal.y) /
           static_cast<float>(sqrt(normal.y * normal.y + normal.z * normal.z));
      nb = glm::cross(nx, normal);
    }

    mat3 basis(nx, normal, nb);
    int count = 0;
    for (int i = 0; i < sampleCount; i++) {

      // generate random direction
      float theta = drand48() * M_PI;
      float cosTheta = cos(theta);
      float sinTheta = sin(theta);
      float dist = drand48();
      vec3 direction = glm::normalize(vec3(cosTheta, sinTheta, dist));

      Ray bounce;
      bounce.position = ray.collisionLocation;
      bounce.direction = direction * basis;
      // return this + new collision point
      if (ClosestIntersection(bounce)) {
        count++;
        indirectLight += cosTheta * globalIllumination(bounce, bounces - 1);
      }
    }
    return (lightHere + (indirectLight / static_cast<float>(count))) *
           surfaceColour / static_cast<float>(M_PI);
  }
  return vec3(0, 0, 0);
}
