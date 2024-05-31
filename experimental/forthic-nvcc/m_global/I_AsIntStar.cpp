#include "I_AsIntStar.h"


int* AsIntStar(shared_ptr<StackItem> item) {
    if (auto i = dynamic_cast<I_AsIntStar*>(item.get())) {
        return i->AsIntStar();
    }
    else {
        throw item->StringRep() + ": does not implement I_AsIntStar";
    }
}
