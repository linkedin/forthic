#pragma once
#include <memory>
#include <string>

#include "I_AsModule.h"
#include "../StackItem.h"
#include "../Module.h"

using namespace std;


class S_Module : public StackItem, public I_AsModule
{
public:
    S_Module(shared_ptr<Module> mod) : mod(mod) {};
    virtual ~S_Module() {};
    shared_ptr<Module> AsModule();
    virtual string AsString();

protected:
    shared_ptr<Module> mod;
};
