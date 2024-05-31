#pragma once

#include <memory>
#include <string>
#include "StackItem.h"
#include "Word.h"

using namespace std;

class Interpreter;

class W_PushItem : public Word
{
public:
    W_PushItem(string name, shared_ptr<StackItem> item);
    virtual void Execute(Interpreter *interp);

protected:
    shared_ptr<StackItem> item;
};
