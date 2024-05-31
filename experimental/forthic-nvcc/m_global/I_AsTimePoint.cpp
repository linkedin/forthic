#include "I_AsTimePoint.h"


high_resolution_clock::time_point AsTimePoint(shared_ptr<StackItem> item) {
    if (auto i = dynamic_cast<I_AsTimePoint*>(item.get())) {
        return i->AsTimePoint();
    }
    else {
        throw item->StringRep() + " does not implement I_AsTimePoint";
    }
}
