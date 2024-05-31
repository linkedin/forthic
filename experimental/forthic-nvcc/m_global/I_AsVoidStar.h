#pragma once
#include <memory>

#include "../StackItem.h"

class I_AsVoidStar {
public:
    virtual void* AsVoidStar() = 0;
};

void* AsVoidStar(shared_ptr<StackItem> item);