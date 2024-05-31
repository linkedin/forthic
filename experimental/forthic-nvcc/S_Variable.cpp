#include "S_Variable.h"

using namespace std;

shared_ptr<StackItem> S_Variable::GetValue() {
    return value;
}

void S_Variable::SetValue(shared_ptr<StackItem> new_value) {
    value = new_value;
}

string S_Variable::StringRep() {
    string value_str = "nullptr";
    if (value != nullptr)   value_str = value->StringRep();
    string result = "S_Variable: ";
    result += value_str;
    return result;
}

string S_Variable::AsString() {
    return StringRep();
}
