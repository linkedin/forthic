#pragma once
#include <string>

#include "../StackItem.h"
#include "I_AsDim3.h"

using namespace std;


class S_Dim3 : public StackItem, public I_AsDim3
{
public:
    S_Dim3(dim3 value) : value(value) {};
    dim3 AsDim3();

    virtual string StringRep();
    virtual string AsString();

protected:
    dim3 value;
};
