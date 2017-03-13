#include "obj_reader.h"

const shared_ptr<const vector<Triangle>> loadObj(string filename) {

	shared_ptr<vector<Triangle>> triangles(new vector<Triangle>());
	shared_ptr<Material> pink(new Material(2, 0.04f, 0.7f));
	pink->loadPNG("red.png");
	vec3 red(0.75f, 0.15f, 0.15f);


	ifstream objFile;
	objFile.open(filename);
	cout << "opened " << filename;

	string line;
	vector<vec3> vertices;
	if(objFile.is_open()){
		while (getline(objFile, line)) {
			if (line[0] == 'v') {
				size_t n1 = line.find(' ');
				size_t n2 = line.find(' ', n1+1);

				string a = line.substr(n1, n2);
				line.erase(0, n2);

				n2 = line.find(' ', 1);
				string b = line.substr(1, n2);
				line.erase(0, n2);

				n2 = line.find(' ', 1);
				string c = line.substr(0, n2);
				line.erase(0, n2);
				vertices.push_back(vec3(stof(a), stof(b), stof(c))*50.f);
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

				Triangle t(vertices[stoi(a)-1], vertices[stoi(b)-1], vertices[stoi(c)-1], vec2(0, 0), vec2(0, 0), vec2(0, 0), red, pink);
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
