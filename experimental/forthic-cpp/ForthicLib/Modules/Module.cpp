#include "pch.h"
#include "Module.h"
#include "Words/PushItemWord.h"


Module::Module(string _name) : name(_name)
{
}


Module::~Module()
{
    // TODO: Module should delete all of its words and variables
}

string Module::ForthicCode()
{
    return "";
}

string Module::GetName()
{
    return name;
}

void Module::AddWord(shared_ptr<Word> word)
{
    words.push_back(shared_ptr<Word>(word));
}

void Module::AddWord(Word* word)
{
    AddWord(shared_ptr<Word>(word));
}

void Module::EnsureVariable(string name)
{
    variables[name];
}

void Module::UseModule(shared_ptr<Module> mod)
{
    using_modules.push_back(mod);
}


shared_ptr<Word> Module::FindWord(string name)
{
    shared_ptr<Word> result = nullptr;
    if (result == nullptr) result = find_in_words(name);          // Find in words
    if (result == nullptr) result = find_variable(name);          // Find varaible
    if (result == nullptr) result = treat_as_literal(name);       // Treat as literal
    if (result == nullptr) result = find_in_using_modules(name);  // Search using modules
    return result;
}


shared_ptr<Word> Module::find_in_words(string name)
{
    for (auto p = words.rbegin(); p != words.rend(); p++)
    {
        if ((*p)->GetName() == name) return (*p);
    }
    return nullptr;
}


shared_ptr<Word> Module::find_variable(string name)
{
    if (variables.find(name) == variables.end())  return nullptr;
    else  return shared_ptr<Word>(new PushItemWord(name, variables[name]));
}

shared_ptr<Word> Module::treat_as_literal(string name)
{
    return nullptr;
}

shared_ptr<Word> Module::find_in_using_modules(string name)
{
    shared_ptr<Word> result = nullptr;
    for (int i = 0; i < using_modules.size(); i++) {
        result = using_modules[i]->FindWord(name);
        if (result != nullptr)  break;
    }
    return result;
}