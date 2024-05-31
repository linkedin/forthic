#include <sstream>
#include "S_Int.h"


shared_ptr<S_Int> S_Int::New(int value) {
    return shared_ptr<S_Int>(new S_Int(value));
}

int S_Int::AsInt() {
    return value;
}

float S_Int::AsFloat() {
    return float(value);
}

string S_Int::StringRep() {
    stringstream builder;
    builder << "S_Int: " << value;
    return builder.str();
}

string S_Int::AsString() {
    stringstream builder;
    builder << value;
    return builder.str();
}
