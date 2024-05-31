#pragma once

#include <memory>
#include <string>
#include <stack>
#include <vector>

#include "StackItem.h"
#include "Token.h"
#include "Word.h"
#include "Module.h"
#include "W_Definition.h"
#include "./m_global/M_Global.h"

using namespace std;

class Interpreter
{
public:
    Interpreter();
    ~Interpreter();
    void Run(string input);

    void StackPush(shared_ptr<StackItem> item);
    shared_ptr<StackItem> StackPop();
    int StackSize();

    void ContextPush(shared_ptr<Module> context);
    shared_ptr<Module> ContextTop();
    void ContextPop();
    int ContextSize();

    shared_ptr<Module> CurModule();
    shared_ptr<Module> ParentModule();
    void RegisterModule(shared_ptr<Module> mod);

    shared_ptr<Word> FindWord(string name);

protected:
    bool is_compiling;

    stack<shared_ptr<StackItem>> param_stack;
    vector<shared_ptr<Module>> module_stack;
    stack<shared_ptr<Module>> context_stack;

    void handle_token(Token tok);
    void handle_STRING(Token tok);
    void handle_START_ARRAY(Token token);
    void handle_END_ARRAY(Token token);
    void handle_START_MODULE(Token tok);
    void handle_END_MODULE(Token tok);
    void handle_START_DEFINITION(Token tok);
    void handle_END_DEFINITION(Token tok);
    void handle_WORD(Token tok);

    void handle_Word(shared_ptr<Word> word);

    shared_ptr<Module> find_module(string name);
    void module_stack_push(shared_ptr<Module> mod);
    map<string, shared_ptr<Module>> registered_modules;

    shared_ptr<W_Definition> cur_definition;
    shared_ptr<Word> find_word(string name);
    shared_ptr<Word> find_registered_module_word(string name);

    M_Global global_module;
};

