#include "pch.h"
#include "GlobalModuleWords.h"
#include "GlobalItemGetters.h"
#include "Interpreter.h"

PopWord::PopWord(string name) : Word(name)
{
}


void PopWord::Execute(Interpreter *interp)
{
    interp->StackPop();
}


UseModulesWord::UseModulesWord(string name) : Word(name)
{
}

// ( modules -- )
void UseModulesWord::Execute(Interpreter *interp)
{
    auto item = interp->StackPop();
    vector<shared_ptr<StackItem>> modules = ForthicGetArray(item.get());
    for (int i = 0; i < modules.size(); i++) {
        shared_ptr<Module> m = ForthicGetModule(modules[i].get());
        interp->CurModule()->UseModule(m);
    }
}

