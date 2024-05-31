#include <sstream>
#include <iostream>

#include "Interpreter.h"
#include "Tokenizer.h"
#include "S_String.h"
#include "W_PushItem.h"
#include "S_StartArray.h"
#include "W_EndArray.h"
#include "m_global/S_Module.h"

Interpreter::Interpreter() : is_compiling(false)
{
    // The first module in the module_stack is the initial local module
    module_stack.push_back(shared_ptr<Module>(new Module("")));
}


Interpreter::~Interpreter()
{
}


shared_ptr<Module> Interpreter::CurModule()
{
    return module_stack.back();
}

shared_ptr<Module> Interpreter::ParentModule() {
    if (module_stack.size() < 2)    return shared_ptr<Module>(&global_module);
    else                            return module_stack[module_stack.size() - 2];
}

void Interpreter::Run(string input)
{
    Tokenizer tokenizer(input);
    Token tok = tokenizer.NextToken();
    while (tok.GetType() != TokenType::EOS)
    {
        handle_token(tok);
        tok = tokenizer.NextToken();
    }
}


void Interpreter::StackPush(shared_ptr<StackItem> item)
{
    param_stack.push(item);
}

shared_ptr<StackItem> Interpreter::StackPop()
{
    if (param_stack.size() == 0)   throw "Stack underflow";
    shared_ptr<StackItem> result = param_stack.top();
    param_stack.pop();
    return result;
}


int Interpreter::StackSize() {
    return param_stack.size();
}


void Interpreter::ContextPush(shared_ptr<Module> context) {
    context_stack.push(context);
}


shared_ptr<Module> Interpreter::ContextTop() {
    if (context_stack.size() == 0)   return nullptr;
    else                             return context_stack.top();
}


void Interpreter::ContextPop() {
    if (context_stack.size() == 0)   throw "Context Stack underflow";
    context_stack.pop();
}


int Interpreter::ContextSize() {
    return context_stack.size();
}


void Interpreter::RegisterModule(shared_ptr<Module> mod)
{
    registered_modules[mod->GetName()] = mod;
    this->Run(mod->ForthicCode());
}


shared_ptr<Word> Interpreter::FindWord(string name)
{
    return find_word(name);
}


void Interpreter::handle_token(Token token)
{
    switch (token.GetType())
    {
    case TokenType::START_ARRAY:
        handle_START_ARRAY(token);
        break;

    case TokenType::END_ARRAY:
        handle_END_ARRAY(token);
        break;

    case TokenType::STRING:
        handle_STRING(token);
        break;

    case TokenType::START_MODULE:
        handle_START_MODULE(token);
        break;

    case TokenType::END_MODULE:
        handle_END_MODULE(token);
        break;

    case TokenType::START_DEFINITION:
        handle_START_DEFINITION(token);
        break;

    case TokenType::END_DEFINITION:
        handle_END_DEFINITION(token);
        break;

    case TokenType::WORD:
        handle_WORD(token);
        break;

    case TokenType::COMMENT:
        break;

    default:
        ostringstream message;
        message << "Unhandled token type: " << (int)(token.GetType());
        throw message.str();
    }
}

void Interpreter::handle_STRING(Token tok)
{
    S_String* item = new S_String(tok.GetText());
    auto word = shared_ptr<Word>(new W_PushItem("<string>", shared_ptr<StackItem>(item)));
    handle_Word(word);
}


void Interpreter::handle_Word(shared_ptr<Word> word)
{
    if (is_compiling)  cur_definition->CompileWord(word);
    else word->Execute(this);
}


void Interpreter::handle_START_ARRAY(Token token)
{
    S_StartArray* item = new S_StartArray();
    auto word = shared_ptr<Word>(new W_PushItem("[", shared_ptr<StackItem>(item)));
    handle_Word(word);
}


void Interpreter::handle_END_ARRAY(Token token)
{
    auto word = shared_ptr<Word>(new W_EndArray("]"));
    handle_Word(word);
}


void Interpreter::handle_START_MODULE(Token tok)
{
    // If module has been registered, push it onto the module stack
    if (auto mod = find_module(tok.GetText()))  module_stack_push(mod);

    // Else if the module has no name, push an anonymous module
    else if (tok.GetText() == "")  module_stack_push(shared_ptr<Module>(new Module("")));

    // Else, register a new module under the specified name and push it onto the module stack
    else
    {
        mod = shared_ptr<Module>(new Module(tok.GetText()));
        RegisterModule(mod);
        module_stack_push(mod);
    }
}


void Interpreter::handle_END_MODULE(Token tok)
{
    module_stack.pop_back();
}


shared_ptr<Module> Interpreter::find_module(string name)
{
    if (registered_modules.find(name) == registered_modules.end()) return nullptr;
    else return registered_modules[name];
}

void Interpreter::module_stack_push(shared_ptr<Module> mod)
{
    module_stack.push_back(mod);
}


void Interpreter::handle_START_DEFINITION(Token tok)
{
    if (is_compiling) throw "Can't have nested definitions";
    cur_definition = shared_ptr<W_Definition>(new W_Definition(tok.GetText(), CurModule()));
    is_compiling = true;
}


void Interpreter::handle_END_DEFINITION(Token tok)
{
    if (!is_compiling) throw "Unmatched end definition";
    CurModule()->AddWord(cur_definition);
    is_compiling = false;
}


void Interpreter::handle_WORD(Token tok)
{
    shared_ptr<Word> word = find_word(tok.GetText());
    if (word == nullptr) throw (string("Unknown word: ") + tok.GetText());
    handle_Word(word);
}

shared_ptr<Word> Interpreter::find_word(string name)
{
    shared_ptr<Word> result = nullptr;

    // Search current context
    if (result == nullptr) {
        shared_ptr<Module> context = ContextTop();
        if (context != nullptr)    result = context->FindWord(name);
    }

    // Search module stack
    if (result == nullptr) {
        for (auto iter = module_stack.rbegin(); iter != module_stack.rend(); iter++)
        {
            result = (*iter)->FindWord(name);
            if (result != nullptr) break;
        }
    }

    // Treat as registered module
    if (result == nullptr)   result = find_registered_module_word(name);

    // Check global module
    if (result == nullptr)   result = global_module.FindWord(name);

    return result;
}


shared_ptr<Word> Interpreter::find_registered_module_word(string name)
{
    auto mod = find_module(name);
    if (mod == nullptr)  return nullptr;
    else  return shared_ptr<Word>(new W_PushItem(mod->GetName(), shared_ptr<S_Module>(new S_Module(mod))));
}
