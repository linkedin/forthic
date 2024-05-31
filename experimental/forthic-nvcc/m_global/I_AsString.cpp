#include "I_AsString.h"

string AsString(shared_ptr<StackItem> item)
{
    if (auto i = dynamic_cast<I_AsString*>(item.get()))
    {
        return i->AsString();
    }
    else
    {
        throw "Item does not implement I_AsString";
    }
}
