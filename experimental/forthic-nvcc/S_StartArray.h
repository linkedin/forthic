#pragma once
#include "StackItem.h"

using namespace std;

class S_StartArray : public StackItem
{
public:
    S_StartArray();
    virtual ~S_StartArray();
    virtual string AsString();
};
