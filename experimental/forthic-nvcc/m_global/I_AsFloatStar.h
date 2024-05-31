#pragma once
#include <memory>

#include "../StackItem.h"

class I_AsFloatStar {
public:
    virtual float* AsFloatStar() = 0;
};

float* AsFloatStar(shared_ptr<StackItem> item);