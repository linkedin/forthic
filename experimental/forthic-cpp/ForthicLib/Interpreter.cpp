#include "pch.h"
#include "Interpreter.h"
#include "Tokenizer.h"
#include "StackItems/StringItem.h"
#include "StackItems/StartArrayItem.h"
#include "Words/PushItemWord.h"
#include "Words/EndArrayWord.h"
#include "StackItems/ModuleItem.h"

Interpreter::Interpreter() : is_compiling(false)
{
    // The first module in the module_stack is the initial local module
    module_stack.push_back(shared_ptr<Module>(new Module("")));
}


Interpreter::~Interpreter()
{
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


shared_ptr<StackItem> Interpreter::StackPop()
{
    shared_ptr<StackItem> result = param_stack.top();
    param_stack.pop();
    return result;
}


void Interpreter::StackPush(shared_ptr<StackItem> item)
{
    param_stack.push(item);
}


shared_ptr<Module> Interpreter::CurModule()
{
    return module_stack.back();
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

    case TokenType::COMMENT:
        break;

    case TokenType::WORD:
        handle_WORD(token);
        break;

    default:
        throw "Unknown token type";
    }
}


void Interpreter::handle_STRING(Token tok)
{
    StringItem* item = new StringItem(tok.GetText());
    handle_Word(new PushItemWord("<string>", shared_ptr<StackItem>(item)));
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


void Interpreter::handle_START_ARRAY(Token token)
{
    StartArrayItem* item = new StartArrayItem();
    handle_Word(new PushItemWord("[", shared_ptr<StackItem>(item)));
}


void Interpreter::handle_END_ARRAY(Token token)
{
    handle_Word(new EndArrayWord("]"));
}


void Interpreter::handle_START_DEFINITION(Token tok)
{
    if (is_compiling) throw "Can't have nested definitions";
    cur_definition = shared_ptr<DefinitionWord>(new DefinitionWord(tok.GetText()));
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

    // Search module stack
    for (auto iter = module_stack.rbegin(); iter != module_stack.rend(); iter++)
    {
        result = (*iter)->FindWord(name);
        if (result != nullptr) break;
    }

    // Treat as registered module
    if (result == nullptr)   result = find_registered_module_word(name);

    // Check global module
    if (result == nullptr)   result = global_module.FindWord(name);

    return result;
}


void Interpreter::handle_Word(shared_ptr<Word> word)
{
    if (is_compiling)  cur_definition->CompileWord(word);
    else word->Execute(this);
}


void Interpreter::handle_Word(Word* word)
{
    handle_Word(shared_ptr<Word>(word));
}


shared_ptr<Module> Interpreter::find_module(string name)
{
    if (registered_modules.find(name) == registered_modules.end()) return nullptr;
    else return registered_modules[name];
}

shared_ptr<Word> Interpreter::find_registered_module_word(string name)
{
    auto mod = find_module(name);
    if (mod == nullptr)  return nullptr;
    else  return shared_ptr<Word>(new PushItemWord(mod->GetName(), new ModuleItem(mod)));
}


void Interpreter::RegisterModule(shared_ptr<Module> mod)
{
    registered_modules[mod->GetName()] = mod;
    this->Run(mod->ForthicCode());
}


void Interpreter::module_stack_push(shared_ptr<Module> mod)
{
    module_stack.push_back(mod);
}