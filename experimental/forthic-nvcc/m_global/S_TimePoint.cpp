#include <sstream>
#include "S_TimePoint.h"

shared_ptr<S_TimePoint> S_TimePoint::New(high_resolution_clock::time_point value) {
    return shared_ptr<S_TimePoint>(new S_TimePoint(value));
}

high_resolution_clock::time_point S_TimePoint::AsTimePoint() {
    return value;
}


string S_TimePoint::StringRep() {
    stringstream builder;
    builder << "S_TimePoint";
    return builder.str();
}

string S_TimePoint::AsString() {
    stringstream builder;
    builder << "S_TimePoint";
    return builder.str();
}
