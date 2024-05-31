#include "I_AsVoidStar.h"

void* AsVoidStar(shared_ptr<StackItem> item) {
    if (auto i = dynamic_cast<I_AsVoidStar*>(item.get())) {
        return i->AsVoidStar();
    }
    else {
        throw item->StringRep() + ": does not implement I_AsVoidStar";
    }
}
