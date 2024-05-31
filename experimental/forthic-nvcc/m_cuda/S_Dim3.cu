#include <sstream>
#include "S_Dim3.h"


dim3 S_Dim3::AsDim3() {
    return value;
}

string S_Dim3::StringRep() {
    stringstream builder;
    builder << "S_Dim3: " << "(" << value.x << ", " << value.y << ", " << value.z << ")";
    return builder.str();
}

string S_Dim3::AsString() {
    stringstream builder;
    builder << "(" << value.x << ", " << value.y << ", " << value.z << ")";
    return builder.str();
}
