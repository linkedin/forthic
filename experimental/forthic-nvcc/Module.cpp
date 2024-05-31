#include <memory>
#include "Module.h"
#include "W_PushItem.h"


Module::Module(string name) : name(name) {
}

shared_ptr<Word> Module::FindWord(string name) {
    shared_ptr<Word> result = nullptr;
    if (result == nullptr)  result = find_in_words(name);
    if (result == nullptr)  result = find_variable(name);
    if (result == nullptr)  result = treat_as_literal(name);
    if (result == nullptr)  result = find_in_using_modules(name);

    return result;
}


string Module::ForthicCode() {
    return "";
}


string Module::GetName() {
    return name;
}


shared_ptr<Word> Module::find_in_words(string name) {
    for (auto p = words.rbegin(); p != words.rend(); p++) {
        if ((*p)->GetName() == name)  return *p;
    }
    return nullptr;
}


shared_ptr<Word> Module::find_variable(string name) {
    if (variables.find(name) == variables.end())  return nullptr;
    else  return shared_ptr<Word>(new W_PushItem(name, variables[name]));
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


void Module::AddWord(shared_ptr<Word> word) {
    words.push_back(word);
}

void Module::AddWord(Word* word) {
    words.push_back(shared_ptr<Word>(word));
}


void Module::EnsureVariable(string name) {
    if (variables.find(name) == variables.end()) {
        variables[name] = shared_ptr<S_Variable>(new S_Variable());
    }
}


void Module::UseModule(shared_ptr<Module> mod) {
    using_modules.push_back(mod);
}

shared_ptr<Word> Module::treat_as_literal(string name)
{
    return nullptr;
}
