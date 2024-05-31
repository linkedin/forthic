#pragma once

#include <string>
#include <stack>
#include <vector>
#include <map>

#include "Word.h"
#include "S_Variable.h"

using namespace std;

class Module {
public:
    Module(string name);

    shared_ptr<Word> FindWord(string name);

    void AddWord(shared_ptr<Word> word);
    void AddWord(Word* word);

    void EnsureVariable(string name);
    void UseModule(shared_ptr<Module> mod);
    virtual string ForthicCode();

    string GetName();

protected:
    string name;
    vector<shared_ptr<Word>> words;
    map<string, shared_ptr<S_Variable>> variables;

protected:
    shared_ptr<Word> find_in_words(string name);
    shared_ptr<Word> find_variable(string name);
    vector<shared_ptr<Module>> using_modules;
    shared_ptr<Word> find_in_using_modules(string name);
    virtual shared_ptr<Word> treat_as_literal(string name);
};

