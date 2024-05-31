#pragma once

#include <memory>

#include "../StackItem.h"

class Module;

class I_AsModule {
public:
    virtual shared_ptr<Module> AsModule() = 0;
};

shared_ptr<Module> AsModule(shared_ptr<StackItem> item);
