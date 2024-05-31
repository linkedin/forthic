#pragma once

#include <string>
#include <stack>
#include <vector>

#include "Defines.h"
#include "StackItems/StackItem.h"
#include "Token.h"
#include "Words/Word.h"
#include "Modules/Module.h"
#include "Words/DefinitionWord.h"
#include "Modules/GlobalModule.h"

using namespace std;

class FORTHICLIB_API Interpreter
{
public:
	Interpreter();
	~Interpreter();
	void Run(string input);
	shared_ptr<StackItem> StackPop();
	void StackPush(shared_ptr<StackItem> item);
    shared_ptr<Module> CurModule();
    void RegisterModule(shared_ptr<Module> mod);

protected:
	bool is_compiling;
	stack<shared_ptr<StackItem>> param_stack;
    vector<shared_ptr<Module>> module_stack;
    map<string, shared_ptr<Module>> registered_modules;
    shared_ptr<DefinitionWord> cur_definition;
    GlobalModule global_module;

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
    void handle_Word(Word* word);

    shared_ptr<Module> find_module(string name);
    void module_stack_push(shared_ptr<Module> mod);

    shared_ptr<Word> find_word(string name);
    shared_ptr<Word> find_registered_module_word(string name);
};

