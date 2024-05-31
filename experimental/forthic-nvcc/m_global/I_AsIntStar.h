#pragma once
#include <memory>

#include "../StackItem.h"

class I_AsIntStar {
public:
    virtual int* AsIntStar() = 0;
};

int* AsIntStar(shared_ptr<StackItem> item);