#pragma once

#include <memory>
#include "../StackItem.h"

class I_AsFloat {
public:
    virtual float AsFloat() = 0;
};

float AsFloat(shared_ptr<StackItem> item);