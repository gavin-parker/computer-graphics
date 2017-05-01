#COMS30115#

##SDL Raytracer and Rasterizer##

##Building and Running##
To build the project navigate to the root folder and run Make on the supplied makefile. To execute the produced .exe, run bin/computer-graphics 'arg' where 'arg' is the mode you want to test. Possible arguments are:

+ ray - ray tracer with AA and phong/diffuse lighting and textures
+ rast - rasterizer with phong/diffuse lighting and textures with experimental shadow map
+ conv - ray tracer with convergent global illumination (you probably want this not gi)
+ gi - ray tracer with global illumination
+ cl - ray tracer with hardware accelerated gi (only compiles on windows by default)
You can also provide a second argument 'teapot' to display a teapot rather than the cornell box.
## Raytracer ##
![raytracer](https://s12.postimg.org/62s50mqlp/screenshot.jpg)
### Color bleeding ###
![raytracer](https://s10.postimg.org/vy9xfviqx/color_bleeding.png")
Features:

+ Texture Mapping
+ Anti-Aliasing
+ Soft shadows
+ Global Illumination via Path Tracing
+ Realtime rendering with OpenCL
+ Acceleration with BVH
+ .obj file loading
+ Reflective Materials

## Rasterizer ##
+ Phong & Diffuse Shading
+ Per pixel / per vertex lighting
+ Texture Mapping
+ (Naff) Shadow Mapping
+ .obj file loading

