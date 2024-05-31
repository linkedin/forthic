#pragma once

#include <string>
#include <stack>
#include <vector>
#include <map>

#include "../Defines.h"
#include "../Modules/GlobalItemGetters.h"
#include "../ForthicLib/Words/Word.h"
#include "../ForthicLib/StackItems/VariableItem.h"

using namespace std;

class FORTHICLIB_API Module
{
public:
    Module(string name);
    virtual ~Module();

    virtual string ForthicCode();

    string GetName();
    void AddWord(shared_ptr<Word> word);
    void AddWord(Word* word);
    void EnsureVariable(string name);
    void UseModule(shared_ptr<Module> mod);

    shared_ptr<Word> FindWord(string name);

protected:
	string name;
    vector<shared_ptr<Word>> words;
    map<string, shared_ptr<VariableItem>> variables;
    vector<shared_ptr<Module>> using_modules;

    shared_ptr<Word> find_in_words(string name);
    shared_ptr<Word> find_variable(string name);
    shared_ptr<Word> find_in_using_modules(string name);
    virtual shared_ptr<Word> treat_as_literal(string name);
};

