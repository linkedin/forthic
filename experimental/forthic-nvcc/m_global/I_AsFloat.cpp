#include "I_AsFloat.h"

float AsFloat(shared_ptr<StackItem> item) {
    if (auto i = dynamic_cast<I_AsFloat*>(item.get())) {
        return i->AsFloat();
    }
    else {
        throw item->StringRep() + " does not implement IGetFloat";
    }
}
