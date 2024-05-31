#include <cstdio>
#include <cmath>
#include "Test.h"

Test::Test() {
}

void Test::printFailure(bool failed, const char* file, int line) {
    if (failed)   printf("=> FAIL  %s:%d\n", file, line);
}

bool Test::isCloseEnough(float l, float r) {
    double tolerance = 1E-5;
    return fabs(l - r) <= tolerance;
}
