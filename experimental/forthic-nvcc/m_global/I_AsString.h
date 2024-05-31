#pragma once
#include <memory>
#include <string>

#include "../StackItem.h"

class I_AsString {
public:
    virtual string AsString() = 0;
};

string AsString(shared_ptr<StackItem> item);

