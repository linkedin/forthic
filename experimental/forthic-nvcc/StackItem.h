#pragma once

#include <string>

using namespace std;

class StackItem
{
public:
    StackItem();
    virtual ~StackItem();
    virtual string StringRep();
    virtual string AsString() = 0;
};

