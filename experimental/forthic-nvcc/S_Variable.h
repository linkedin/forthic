#pragma once
#include <memory>

#include "StackItem.h"

using namespace std;

class S_Variable : public StackItem
{
public:
    S_Variable() : value(nullptr) {};
    virtual ~S_Variable() {};

    shared_ptr<StackItem> GetValue();
    void SetValue(shared_ptr<StackItem> new_value);

    virtual string StringRep();
    virtual string AsString();

protected:
    shared_ptr<StackItem> value;
};

