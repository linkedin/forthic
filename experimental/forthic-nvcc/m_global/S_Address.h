#pragma once
#include <string>

#include "../StackItem.h"
#include "I_AsFloatStar.h"
#include "I_AsIntStar.h"
#include "I_AsVoidStar.h"

using namespace std;


class S_Address : public StackItem, public I_AsFloatStar, public I_AsIntStar, public I_AsVoidStar
{
public:
    S_Address(void* address) : address(address) {};
    static shared_ptr<S_Address> New(void* address);

    float* AsFloatStar();
    int* AsIntStar();
    void* AsVoidStar();

    virtual string StringRep();
    virtual string AsString();

protected:
    void* address;
};
