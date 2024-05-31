#pragma once

#include <cuda_runtime.h>
#include <memory>
#include <string>
#include <stack>
#include <vector>
#include <map>

#include "../Module.h"

using namespace std;


class M_Cuda : public Module
{
public:
    M_Cuda();
    virtual string ForthicCode();

protected:
    // virtual shared_ptr<Word> treat_as_literal(string name);
};

void checkCudaCall(const cudaError_t res, const char* file, int line);