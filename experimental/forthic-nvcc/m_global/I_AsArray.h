#pragma once

#include <memory>
#include <string>
#include <vector>

#include "../StackItem.h"

class I_AsArray {
public:
    virtual vector<shared_ptr<StackItem>> AsArray() = 0;
};

vector<shared_ptr<StackItem>> AsArray(shared_ptr<StackItem> item);