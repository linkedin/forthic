#include "pch.h"
#include "GlobalModule.h"
#include "Words/PushItemWord.h"
#include "StackItems/FloatItem.h"
#include "StackItems/IntItem.h"
#include "GlobalModuleWords.h"

GlobalModule::GlobalModule() : Module("Forthic.global")
{
    AddWord(new PopWord("POP"));
    AddWord(new UseModulesWord("USE-MODULES"));
}


GlobalModule::~GlobalModule()
{
}


shared_ptr<Word> GlobalModule::treat_as_float(string name)
{
    try {
        float value = stof(name);
        return shared_ptr<Word>(new PushItemWord(name, new FloatItem(value)));
    }
    catch (...) {
        return nullptr;
    }
}


shared_ptr<Word> GlobalModule::treat_as_int(string name)
{
    try {
        string::size_type sz;
        int value = stoi(name, &sz);
        char c = name[sz];
        if (c == '.' || c == 'e' || c == 'E') return nullptr;
        else  return shared_ptr<Word>(new PushItemWord(name, new IntItem(value)));
    }
    catch (...) {
        return nullptr;
    }
}


shared_ptr<Word> GlobalModule::treat_as_literal(string name)
{
    shared_ptr<Word> result = nullptr;
    if (result == nullptr)  result = treat_as_int(name);
    if (result == nullptr)  result = treat_as_float(name);
    return result;
}

int FORTHICLIB_API ForthicGetInt(StackItem *item)
{
    if (auto i = dynamic_cast<IGetInt*>(item))
    {
        return i->GetInt();
    }
    else
    {
        throw "Item does not implement IGetInt";
    }
}

float FORTHICLIB_API ForthicGetFloat(StackItem *item)
{
    if (auto i = dynamic_cast<IGetFloat*>(item))
    {
        return i->GetFloat();
    }
    else
    {
        throw "Item does not implement IGetFloat";
    }
}