#include "myrand.h"

#ifndef unix
float RAND() { return ((float)rand() / (RAND_MAX)); }
#else
float RAND() { return drand48(); }
#endif
