#include "I_AsFloatStar.h"

float* AsFloatStar(shared_ptr<StackItem> item) {
    if (auto i = dynamic_cast<I_AsFloatStar*>(item.get())) {
        return i->AsFloatStar();
    }
    else {
        throw item->StringRep() + ": does not implement I_AsFloatStar";
    }
}