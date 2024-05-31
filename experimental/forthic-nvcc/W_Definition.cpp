#include <cstdio>
#include "W_Definition.h"
#include "Interpreter.h"
#include "Module.h"


W_Definition::W_Definition(string word_name, shared_ptr<Module> module) : Word(word_name), module(module)
{
}

W_Definition::~W_Definition()
{
}

void W_Definition::CompileWord(shared_ptr<Word> word)
{
    words.push_back(word);
}

void W_Definition::Execute(Interpreter *interp)
{
    for (auto iter = words.begin(); iter != words.end(); iter++)
    {
        interp->ContextPush(module);
        (*iter)->Execute(interp);
        interp->ContextPop();
    }
}
