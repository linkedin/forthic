#include <sstream>
#include "S_Address.h"


shared_ptr<S_Address> S_Address::New(void* address) {
    return shared_ptr<S_Address>(new S_Address(address));
}

float* S_Address::AsFloatStar() {
    return (float*)(address);
}

int* S_Address::AsIntStar() {
    return (int*)(address);
}

void* S_Address::AsVoidStar() {
    return (address);
}

string S_Address::StringRep() {
    stringstream builder;
    builder << "S_Address: " << (long int)(address);
    return builder.str();
}

string S_Address::AsString() {
    stringstream builder;
    builder << (long int)(address);
    return builder.str();
}
