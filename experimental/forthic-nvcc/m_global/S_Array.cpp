#include <sstream>
#include "S_Array.h"

vector<shared_ptr<StackItem>> S_Array::AsArray() {
    return items;
}

string S_Array::StringRep() {
    stringstream builder;
    builder << "S_Array(" << items.size() << ")";
    return builder.str();
}


string S_Array::AsString() {
    return StringRep();
}
