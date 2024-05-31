#include "I_AsModule.h"

shared_ptr<Module> AsModule(shared_ptr<StackItem> item)
{
    if (auto i = dynamic_cast<I_AsModule*>(item.get()))
    {
        return i->AsModule();
    }
    else
    {
        throw "Item does not implement IAsModule";
    }
}