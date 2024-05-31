#include <sstream>
#include "S_Float.h"


S_Float::S_Float(float _value) : value(_value)
{
}


S_Float::~S_Float()
{
}

float S_Float::AsFloat() {
    return value;
}

int S_Float::AsInt() {
    return int(value);
}


string S_Float::StringRep() {
    stringstream builder;
    builder << "S_Float: " << value;
    return builder.str();
}

string S_Float::AsString() {
    stringstream builder;
    builder << value;
    return builder.str();
}
