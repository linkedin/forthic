#include "S_CudaDeviceProp.h"

shared_ptr<S_CudaDeviceProp> S_CudaDeviceProp::New(cudaDeviceProp value) {
    return shared_ptr<S_CudaDeviceProp>(new S_CudaDeviceProp(value));
}

const cudaDeviceProp& S_CudaDeviceProp::deviceProp() {
    return value;
}

string S_CudaDeviceProp::StringRep() {
    return "S_CudaDeviceProp";
}

string S_CudaDeviceProp::AsString() {
    return "S_CudaDeviceProp";
}
