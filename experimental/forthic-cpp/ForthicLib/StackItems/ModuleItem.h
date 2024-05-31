#pragma once
#include <string>

#include "../Defines.h"
#include "../Modules/GlobalItemGetters.h"
#include "StackItem.h"
#include "../Modules/Module.h"

using namespace std;


class FORTHICLIB_API ModuleItem : public StackItem, public IGetModule
{
public:
	ModuleItem(Module* mod);
	ModuleItem(shared_ptr<Module> mod);
	virtual ~ModuleItem();
	shared_ptr<Module> GetModule();

protected:
	shared_ptr<Module> mod;
};
