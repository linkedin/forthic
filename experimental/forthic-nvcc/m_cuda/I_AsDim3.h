#pragma once
#include <memory>

#include <cuda_runtime.h>

#include "../StackItem.h"

class I_AsDim3 {
public:
    virtual dim3 AsDim3() = 0;
};


dim3 AsDim3(shared_ptr<StackItem> item);
