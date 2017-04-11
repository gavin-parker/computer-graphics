#include "material.h"

Material::Material()
    : Material(vec3(0.1, 0.1, 0.1), vec3(0.75, 0.75, 0.75),
               vec3(0.75, 0.75, 0.75), 500, false, false) {}

Material::Material(vec3 ka, vec3 kd, vec3 ks, vec4::value_type ns,
                   bool isMirrored, bool isRefractive)
    : Material(ka, kd, ks, ns, "", "", "", "", isMirrored, isRefractive) {}

Material::Material(vec3 ka, vec3 kd, vec3 ks, vec4::value_type ns,
                   const string &mapKa, const string &mapKd,
                   const string &mapKs, const string &mapNs, bool isMirrored,
                   bool isRefractive)
    : ambientTexture(vec4(ka, 1.0f), mapKa),
      diffuseTexture(vec4(kd, 1.0f), mapKd),
      specularTexture(vec4(ka, 1.0f), mapKa),
      specularExponentTexture(vec4(1.0f, 1.0f, 1.0f, ns), mapNs),
      isMirrored(isMirrored), isRefractive(isRefractive), brdf(BRDF()){}

vec3 Material::ambient() const { return vec3(ambientTexture.getScale()); }

vec3 Material::ambient(vec2 uv) const { return vec3(ambientTexture[uv]); }

vec3 Material::diffuse() const { return vec3(diffuseTexture.getScale()); }

vec3 Material::diffuse(vec2 uv) const { return vec3(diffuseTexture[uv]); }

vec3 Material::specular() const { return vec3(specularTexture.getScale()); }

vec3 Material::specular(vec2 uv) const { return vec3(specularTexture[uv]); }


Material::Material(BRDF &brdf) : brdf(brdf) {
	useBRDF = true;
};

vec3 Material::getReflection(vec3 incidence, vec3 view, vec3 normal) const {
	if (useBRDF) {
		return brdf.getLight(incidence, view, normal);
	}
	else {
		return glm::normalize(diffuse());
	}
}
float Material::specularExponent() const {
  return specularExponentTexture.getScale().a;
}

float Material::specularExponent(vec2 uv) const {
  return specularExponentTexture[uv].a;
}
