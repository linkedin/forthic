#pragma once
#include <string>

#include "../StackItem.h"

#include "I_AsInt.h"
#include "I_AsFloat.h"

using namespace std;


class S_Int : public StackItem, public I_AsInt, public I_AsFloat
{
public:
    S_Int(int value) : value(value) {};
    static shared_ptr<S_Int> New(int value);

    int AsInt();
    float AsFloat();

    virtual string StringRep();
    virtual string AsString();

protected:
    int value;
};
