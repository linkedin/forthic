#pragma once

#include <memory>
#include <chrono>

#include "../StackItem.h"

using namespace std;
using namespace std::chrono;


class I_AsTimePoint {
public:
    virtual high_resolution_clock::time_point AsTimePoint() = 0;
};

high_resolution_clock::time_point AsTimePoint(shared_ptr<StackItem> item);
