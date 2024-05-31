#pragma once
#include <string>
#include <chrono>

#include "I_AsTimePoint.h"
#include "../StackItem.h"

using namespace std;
using namespace std::chrono;


class S_TimePoint : public StackItem, public I_AsTimePoint
{
public:
    S_TimePoint(high_resolution_clock::time_point value) : value(value) {};
    static shared_ptr<S_TimePoint> New(high_resolution_clock::time_point value);

    high_resolution_clock::time_point AsTimePoint();

    virtual string StringRep();
    virtual string AsString();

protected:
    high_resolution_clock::time_point value;
};
