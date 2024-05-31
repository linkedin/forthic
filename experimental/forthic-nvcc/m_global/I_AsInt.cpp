#include "I_AsInt.h"

int AsInt(shared_ptr<StackItem> item) {
    if (auto i = dynamic_cast<I_AsInt*>(item.get())) {
        return i->AsInt();
    }
    else {
        throw item->StringRep() + " does not implement IGetInt";
    }
}
