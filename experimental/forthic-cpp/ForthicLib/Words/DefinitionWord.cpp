#include "pch.h"
#include "DefinitionWord.h"
#include "../Interpreter.h"


DefinitionWord::DefinitionWord(string word_name) : Word(word_name)
{
}

DefinitionWord::~DefinitionWord()
{
}

void DefinitionWord::CompileWord(shared_ptr<Word> word)
{
    words.push_back(word);
}

void DefinitionWord::Execute(Interpreter *interp)
{
    for (auto iter = words.begin(); iter != words.end(); iter++)
    {
        (*iter)->Execute(interp);
    }
}
