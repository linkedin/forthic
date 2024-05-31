#pragma once
#include <string>
#include <vector>
#include "../Defines.h"
#include "../StackItems/StackItem.h"
#include "Word.h"

using namespace std;

class Interpreter;

class FORTHICLIB_API DefinitionWord : public Word
{
public:
    DefinitionWord(string name);
    virtual ~DefinitionWord();
    virtual void Execute(Interpreter *interp);

    void CompileWord(shared_ptr<Word> word);

protected:
    vector<shared_ptr<Word>> words;
};

