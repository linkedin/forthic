#include "S_Module.h"


shared_ptr<Module> S_Module::AsModule() {
    return mod;
}


string S_Module::AsString() {
    return "S_Module";
}
