#include "obj_reader.h"

const shared_ptr<const vector<Triangle>> loadObj(string filename, float scale_factor, bool inCornell) {

	vec3 center;

	shared_ptr<vector<Triangle>> triangles;
	
	if (inCornell) {
		triangles = loadTestModel();
		center = vec3(277, 250, 277);
	}
	shared_ptr<Material> pink(new Material(2, 0.04f, 0.7f));
	pink->loadPNG("red.png");
	vec3 yellow(0.75f, 0.75f, 0.15f);


	ifstream objFile;
	objFile.open(filename);
	cout << "opened " << filename;

	string line;
	vector<vec3> vertices;
	vector<vec3> textureVerts;
	vector<vec3> vertexNormals;

	if(objFile.is_open()){
		while (getline(objFile, line)) {
			if (line[0] == 'v' && line[1] == ' ') {
				char mode = line[1];


				size_t n1 = line.find(' ');
				size_t n2 = line.find(' ', n1+2);

				string a = line.substr(n1, n2);
				line.erase(0, n2);

				n2 = line.find(' ', 1);
				string b = line.substr(1, n2);
				line.erase(0, n2);

				n2 = line.find(' ', 1);
				string c = line.substr(0, n2);
				line.erase(0, n2);
				if (mode == ' ') {
					vertices.push_back(vec3(stof(a), stof(b), stof(c))*scale_factor + center);
				}
				else if(mode == 't') {
					textureVerts.push_back(vec3(stof(a), stof(b), stof(c)));
				}
				else if (mode == 'n') {
					vertexNormals.push_back(vec3(stof(a), stof(b), stof(c)));
				}
			}

			if (line[0] == 'f') {

				size_t n1 = line.find(' ');
				size_t n2 = line.find(' ', n1 + 1);

				string a = line.substr(n1, n2);
				line.erase(0, n2);

				n2 = line.find(' ', 1);
				string b = line.substr(1, n2);
				line.erase(0, n2);

				n2 = line.find(' ', 1);
				string c = line.substr(0, n2);
				line.erase(0, n2);

				Triangle t(vertices[stoi(c)-1], vertices[stoi(b)-1], vertices[stoi(a)-1], vec2(0, 0), vec2(0, 0), vec2(0, 0), yellow, pink);
				triangles->push_back(t);
			}


			//if (vertices.size() == 3) {
			//
			//	Triangle t(vertices[0], vertices[1], vertices[2], vec2(0, 0), vec2(0, 0), vec2(0, 0), red, pink);
			//	triangles->push_back(t);
			//	vertices.clear();
			//}
		}
	}
	else {
		cout << "could not load a file \n";
	}

	objFile.close();

	cout << "triangles loaded: " << triangles->size();
	return triangles;
}
