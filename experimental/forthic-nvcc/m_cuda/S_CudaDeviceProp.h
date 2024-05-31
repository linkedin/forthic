#pragma once
#include <string>
#include <memory>
#include <cuda_runtime.h>

#include "../StackItem.h"

using namespace std;

class S_CudaDeviceProp : public StackItem
{
public:
    S_CudaDeviceProp(cudaDeviceProp value) : value(value) {};
    static shared_ptr<S_CudaDeviceProp> New(cudaDeviceProp value);

    const cudaDeviceProp& deviceProp();

    virtual string StringRep();
    virtual string AsString();

protected:
    cudaDeviceProp value;
};
