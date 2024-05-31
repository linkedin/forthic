#include "S_String.h"

shared_ptr<S_String> S_String::New(string s) {
    return shared_ptr<S_String>(new S_String(s));
}

string S_String::AsString() {
    return item_string;
}

string S_String::StringRep() {
    return item_string;
}
