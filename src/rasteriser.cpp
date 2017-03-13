#include "rasteriser.h"

Rasteriser::Rasteriser(int width, int height, shared_ptr<LightingEngine> lighting, shared_ptr<Scene> scene,vec3 cameraPos, bool useShadows, bool fullscreen)
	: SdlScreen(width, height, fullscreen), depthBuffer(width * height), shadowBuffer(6* 128 * 128),
	triangles(scene->triangles),
	camera(cameraPos, 0.0f, 30.0f), light(scene->light), lighting(lighting), leftBuffer(triangles->size()), rightBuffer(triangles->size()), clipped_triangles(vector<Triangle>()) {}

void Rasteriser::update(float dt) {
	camera.update(dt);
	light->update(dt);
}

void Rasteriser::draw(int width, int height) {
	clipped_triangles.clear();
	clip(width, height);
	leftBuffer.resize(clipped_triangles.size());
	rightBuffer.resize(clipped_triangles.size());

	for (size_t i = 0; i < depthBuffer.size(); ++i) {
		depthBuffer[i] = numeric_limits<float>::max();
	}
	float *shadow_ptr = &shadowBuffer[0];
#pragma omp parallel for
	for (int i = 0; (size_t)i < shadowBuffer.size(); ++i) {
		shadow_ptr[i] = numeric_limits<float>::max();
	}
	for (size_t t = 0; t < clipped_triangles.size(); t++) {

		const Triangle &triangle = (clipped_triangles)[t];
		vector<Vertex> vertices = {
			Vertex(triangle.v0, triangle.normal, vec2(1, 1), triangle.colour),
			Vertex(triangle.v1, triangle.normal, vec2(1, 1), triangle.colour),
			Vertex(triangle.v2, triangle.normal, vec2(1, 1), triangle.colour) };
		vector<Pixel> proj(vertices.size());

		//here is where we do our vertex shading
		for (size_t i = 0; i < vertices.size(); i++) {
			Ray ray;
			ray.collisionLocation = vertices[i].position;
			ray.collision = &triangle;
			ray.direction = camera.position - vertices[i].position;
			vec3 illumination = lighting->calculateLight(ray);
			proj[i] = VertexShader(vertices[i], width, height);
			proj[i].v.illumination = illumination;

		}

		int projE1X = proj[1].x - proj[0].x;
		int projE1Y = proj[1].y - proj[0].y;
		int projE2X = proj[2].x - proj[0].x;
		int projE2Y = proj[2].y - proj[0].y;

		if (projE1X * projE2Y > projE2X * projE1Y) {
			vector<Pixel> leftPixels;
			vector<Pixel> rightPixels;
			computePolygonRows(proj, leftPixels, rightPixels, triangle);
			//drawPolygonRows(width, height, leftPixels, rightPixels, triangle);
			shadowPass(width, height, leftPixels, rightPixels, triangle);
			leftBuffer[t] = leftPixels;
			rightBuffer[t] = rightPixels;
		}
	}
	for (size_t t = 0; t < clipped_triangles.size(); t++) {
		const Triangle &triangle = (clipped_triangles)[t];
		vector<Pixel> leftPixels = leftBuffer[t];
		vector<Pixel> rightPixels = rightBuffer[t];
		drawPolygonRows(width, height, leftPixels, rightPixels, triangle);
	}
}

int computeClipping(vec4 point, vec2 bounds) {
	int clipping = INSIDE;

	if (point.x < -abs(point.w*bounds.x)) {
		clipping |= LEFT;
	}
	else if (point.x > abs(point.w*bounds.x)) {
		clipping |= RIGHT;
	}
	if (point.y < -abs(point.w*bounds.y)) {
		clipping |= BOTTOM;
	}
	else if (point.y > abs(point.w*bounds.y)) {
		clipping |= TOP;
	}
	if (point.z < 10.f) {
		clipping |= NEAR;
	}
	return clipping;

}

Line CohenSutherland(vec4 A, vec4 B, vec2 bounds, int &draw) {
	int clipA = computeClipping(A, bounds);
	int clipB = computeClipping(B, bounds);
	Line ab = { A, B };

	if (!(clipA | clipB)) {
		draw = 2;
		return{ A,B };
	}
	int clipCount = 0;
	while (true) {
		if (clipCount > 4) {
			cout << "oh bugger \n";
			return{ A,B };
		}
		clipCount++;
		//line within screen
		if (!(clipA | clipB)) {
			draw = 1;
			break;
		}
		//line out of screen
		else if (clipA & clipB) {
			draw = 0;
			break;
		}
		else {
			float x, y, z;
			vec3 newPoint;
			int clippedPoint = clipA ? clipA : clipB;

			if (clippedPoint & TOP) {
				//x = A.x + (B.x - A.x) * (bounds.y - A.y) / (B.y - A.y);
				y = bounds.y;

			}
			else if (clippedPoint & BOTTOM) {
				//x = A.x + (B.x - A.x) * ((-bounds.y) - A.y) / (B.y - A.y);
				y = (-bounds.y);
			}
			else if (clippedPoint & RIGHT) {
				//y = A.y + (B.y - A.y) * (bounds.x - A.x) / (B.x - A.x);
				x = bounds.x;
			}
			else if (clippedPoint & LEFT) {
				//y = A.y + (B.y - A.y) * ((-bounds.x) - A.x) / (B.x - A.x);
				x = (-bounds.x);
			}
			if (clippedPoint & NEAR) {
				z = 10.f;

			}
			if (clippedPoint == clipA) {
				A = pointOnLine(ab, ivec2(x, y));
				if (clippedPoint & TOP || clippedPoint & BOTTOM) {
					float newY = glm::length(ab.a.y - y) / glm::length(ab.a.y - ab.b.y);
					
				}


				clipA = computeClipping(A, bounds);
			}
			else {
				B = pointOnLine(ab, ivec2(x, y));
				clipB = computeClipping(B,bounds);
			}

		}
	}
	return{ A, B };
}

void Rasteriser::clip(int width, int height) {
	for (size_t t = 0; t < triangles->size(); t++) {
		const Triangle &triangle = (*triangles)[t];
		vector<Vertex> vertices = {
			Vertex(triangle.v0, triangle.normal, vec2(1, 1), triangle.colour),
			Vertex(triangle.v1, triangle.normal, vec2(1, 1), triangle.colour),
			Vertex(triangle.v2, triangle.normal, vec2(1, 1), triangle.colour) };
		
		float xMax = (float)width/2.0;
		float yMax = (float)height / 2.0;

		int clippedVerts = 0;
		int clippings[3] = { 0,0,0 };
		vec4 lines[3];
		std::unordered_set<vec3> newPoints;
		bool clipped = false;
		for (int i = 0; i < 3; i++) {
			vec4 homA = camera.clipSpace(vertices[i]);
			vec4 homB = camera.clipSpace(vertices[(i+1)%3]);
			int draw;
			Line original = { homA, homB };
			if (homA.z > 0 && homB.z > 0) {
				Line newline = CohenSutherland(homA, homB, vec2(xMax, yMax), draw);

				if (draw) {
					newPoints.insert(camera.worldSpace(newline.a));
					newPoints.insert(camera.worldSpace(newline.b));
					if (draw != 2) {
						clipped = true;
					}
				}
			}
		}

		//unique points to vector
		vector<vec3> points;
		for (auto local_it = newPoints.begin(); local_it != newPoints.end(); ++local_it) points.push_back(*local_it);

		if (!clipped) {
			clipped_triangles.push_back(triangle);
		}else if (points.size() == 3) {
			cout << "clipping triangle \n";
			clipped_triangles.push_back(Triangle(points[0], points[1], points[2], vec2(0,0), vec2(0, 0), vec2(0, 0), triangle.colour, triangle.mat));
		}
		else if (points.size() > 3) {
			cout << "split poly for clipping: " << points.size() << " \n";
			for (int i = 0; i < points.size() - 1; i++) {
				clipped_triangles.push_back(Triangle(points[i+1], points[i], points[0], vec2(0, 0), vec2(0, 0), vec2(0, 0), triangle.colour, triangle.mat));
			}
		}
	}
}


void Rasteriser::drawPolygonRows(int width, int height,
	vector<Pixel> &leftPixels,
	vector<Pixel> &rightPixels, const Triangle &triangle) {
	if (leftPixels.size() < 1) {
		return;
	}
	Pixel* l_pixels = &leftPixels[0];
	Pixel* r_pixels = &rightPixels[0];
#pragma omp parallel for
	for (int y = 0; y < static_cast<int>(leftPixels.size()); y++) {

		for (int x = l_pixels[y].x; x <= r_pixels[y].x; x++) {
			float pixelDepth = lerpF(l_pixels[y].depth, r_pixels[y].depth,
				deLerpF(l_pixels[y].x, r_pixels[y].x, x));
			if (pixelDepth > 0 && x >= 0 && x < static_cast<int>(width) &&
				l_pixels[y].y >= 0 && l_pixels[y].y < static_cast<int>(height)) {
				float &bufferDepth = depthBuffer[width * l_pixels[y].y + x];

				if (pixelDepth < bufferDepth) {
					bufferDepth = pixelDepth;
					Vertex pixelVert =
						lerpV(l_pixels[y].v, r_pixels[y].v, l_pixels[y].depth, r_pixels[y].depth, pixelDepth,
							deLerpF(l_pixels[y].x, r_pixels[y].x, x));

					vec3 f1 = triangle.v0 - pixelVert.position;
					vec3 f2 = triangle.v1 - pixelVert.position;
					vec3 f3 = triangle.v2 - pixelVert.position;
					float a = glm::length(glm::cross(triangle.e1, triangle.e2));
					float a1 = glm::length(glm::cross(f2, f3)) / a;
					float a2 = glm::length(glm::cross(f3, f1)) / a;
					float a3 = glm::length(glm::cross(f1, f2)) / a;
					vec3 bary(a1, a2, a3);
					//if calculating per pixel...
					Ray ray;
					vec3 realPos = pixelVert.position;
					ray.direction = camera.position - realPos;
					ray.collisionLocation = realPos;
					ray.collision = &triangle;
					float depth = 0.f;
					indexedPixel lightPixel = light->projectVertex(pixelVert.position, depth);
					vec3 lightColour = vec3(0, 1, 0);//triangle.colour*vec3(0.2,0.2,0.2);

					pixelVert.illumination = light->directLight(ray);
					if (lightPixel.x > -1 && useShadows) {
						int shadowBufferIndex = lightPixel.i*(128 * 128) + 128 * lightPixel.y + lightPixel.x;
						float d = shadowBuffer[shadowBufferIndex];
						float bias = 20.f;
						if (depth <= (d+ 20.f)) {
							lightColour = triangle.colour*pixelVert.illumination;
						}
						else {
							//lightColour = vec3(0, 0, 1);
							lightColour = triangle.colour*vec3(0.1,0.1,0.1);
						}
					}
					else {
						lightColour = triangle.getColour(bary)*pixelVert.illumination;
					}
					drawPixel(x, l_pixels[y].y, vec3(std::min(lightColour.r, 1.0f),
						std::min(lightColour.g, 1.0f),
						std::min(lightColour.b, 1.0f)));
				}
			}
		}
	}
}

void Rasteriser::computePolygonRows(const vector<Pixel> &vertexPixels,
	vector<Pixel> &leftPixels,
	vector<Pixel> &rightPixels, const Triangle &triangle) {

	int max = -numeric_limits<int>::max();
	int min = numeric_limits<int>::max();

	for (size_t i = 0; i < vertexPixels.size(); i++) {
		max = std::max(vertexPixels[i].y, max);
		min = std::min(vertexPixels[i].y, min);
	}
	int rows = max - min + 1;
	if (rows < 2) {
		return;
	}
	for (int i = 0; i < rows; i++) {
		leftPixels.push_back(Pixel(numeric_limits<int>::max(), 0, 0.0f));
		rightPixels.push_back(Pixel(-numeric_limits<int>::max(), 0, 0.0f));
	}
	for (int i = 0; i < 3; i++) {
		const Pixel &start = vertexPixels[i];
		const Pixel &end = vertexPixels[(i + 1) % 3];
		float step =
			1.f / (glm::length(vec2(start.x - end.x, start.y - end.y)) + 1);
		for (float t = 0; t < 1; t += step) {
			Pixel pixel = lerpP(vertexPixels[i], vertexPixels[(i + 1) % 3], t);

			int y = pixel.y - min;
			if (pixel.x < leftPixels[y].x) {
				leftPixels[y] = pixel;
			}
			if (pixel.x > rightPixels[y].x) {
				rightPixels[y] = pixel;
			}
		}
	}
}

void Rasteriser::shadowPass(int width, int height,
	vector<Pixel> &leftPixels,
	vector<Pixel> &rightPixels, const Triangle &triangle) {
	if (leftPixels.size() < 1) {
		return;
	}
	Pixel* l_pixels = &leftPixels[0];
	Pixel* r_pixels = &rightPixels[0];
	//gets the depth in 6 directions from the light source
	for (int y = 0; y < static_cast<int>(leftPixels.size()); y++) {
		for (int x = l_pixels[y].x; x <= r_pixels[y].x; x++) {
			float pixelDepth = lerpF(l_pixels[y].depth, r_pixels[y].depth,
				deLerpF(l_pixels[y].x, r_pixels[y].x, x));
			Vertex pixelVert =
				lerpV(l_pixels[y].v, r_pixels[y].v, l_pixels[y].depth, r_pixels[y].depth, pixelDepth,
					deLerpF(l_pixels[y].x, r_pixels[y].x, x));
			float depth = 0;
			indexedPixel lightPixel = light->projectVertex(pixelVert.position, depth);
			if (lightPixel.x > -1) {
				int shadowBufferIndex = lightPixel.i*(128 * 128) + 128 * lightPixel.y + lightPixel.x;
				//shadowBuffer stores closest depths to light source
				float d = shadowBuffer[shadowBufferIndex];
				if (depth < (d + 10.f)) {
					shadowBuffer[shadowBufferIndex] = depth;
				}
			}
		}
	}

}

Pixel Rasteriser::VertexShader(Vertex v, int width, int height) {
	vec3 camSpace = camera.projectVertex(v);
	return Pixel(static_cast<int>(width * (1 - camSpace.x) / 2.0),
		static_cast<int>(height * (1 - camSpace.y) / 2.0), camSpace.z,
		v);
}
