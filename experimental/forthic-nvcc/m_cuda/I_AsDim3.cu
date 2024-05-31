#include "I_AsDim3.h"


dim3 AsDim3(shared_ptr<StackItem> item) {
    if (auto i = dynamic_cast<I_AsDim3*>(item.get())) {
        return i->AsDim3();
    }
    else {
        throw "Item does not implement I_AsDim3";
    }
}

