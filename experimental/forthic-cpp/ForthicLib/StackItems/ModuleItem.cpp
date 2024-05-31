#include "pch.h"
#include "ModuleItem.h"



ModuleItem::ModuleItem(Module* _mod) : mod(shared_ptr<Module>(_mod))
{
}


ModuleItem::ModuleItem(shared_ptr<Module> _mod) : mod(_mod)
{
}


ModuleItem::~ModuleItem()
{
}

shared_ptr<Module> ModuleItem::GetModule()
{
	return mod;
}
