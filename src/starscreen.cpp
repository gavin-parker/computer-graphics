#include "starscreen.h"

StarScreen::StarScreen(int width, int height, vector<vec3>::size_type starCount,
                       float starVelocity, bool fullscreen)
    : SdlScreen(width, height, fullscreen), starVelocity(starVelocity) {

  stars = vector<vec3>(starCount);

  for (unsigned int i = 0; i < starCount; ++i) {
    float x = lerpF(-1.0f, 1.0f, deLerpF(0.0f, RAND_MAX, rand())),
          y = lerpF(-1.0f, 1.0f, deLerpF(0.0f, RAND_MAX, rand())),
          z = deLerpF(0.0f, RAND_MAX, rand());

    stars[i] = vec3(x, y, z);
  }
}

void StarScreen::update(float dt) {
  // calculate star transform
  for (vec3 &star : stars) {
    star.z += starVelocity * dt;
    if (star.z <= 0) {
      star.z += 1;
    }
    if (star.z >= 0) {
      star.z -= 1;
    }
  }
}

void StarScreen::draw(int width, int height) {
  // calculate projections of stars
  int focal_length = height / 2;
  for (const vec3 &star : stars) {
    int u = focal_length * (star.x / star.z) + width / 2.0f;
    int v = focal_length * (star.y / star.z) + height / 2.0f;
    vec3 color = 0.2f * vec3(1, 1, 1) / (star.z * star.z);

    drawPixel(u, v, color);
  }
}
