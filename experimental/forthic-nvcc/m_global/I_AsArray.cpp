#include "I_AsArray.h"

vector<shared_ptr<StackItem>> AsArray(shared_ptr<StackItem> item)
{
    if (auto i = dynamic_cast<I_AsArray*>(item.get()))
    {
        return i->AsArray();
    }
    else
    {
        throw "Item does not implement I_AsArray";
    }
}
