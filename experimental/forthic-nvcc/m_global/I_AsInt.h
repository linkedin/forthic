#pragma once

#include <memory>
#include "../StackItem.h"

class I_AsInt {
public:
    virtual int AsInt() = 0;
};

int AsInt(shared_ptr<StackItem> item);