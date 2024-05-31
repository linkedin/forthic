#pragma once
#include <string>

#include "../StackItem.h"
#include "I_AsInt.h"
#include "I_AsFloat.h"

using namespace std;


class S_Float : public StackItem, public I_AsFloat, public I_AsInt
{
public:
    S_Float(float value);
    virtual ~S_Float();

    float AsFloat();
    int AsInt();
    virtual string StringRep();
    virtual string AsString();

protected:
    float value;
};
