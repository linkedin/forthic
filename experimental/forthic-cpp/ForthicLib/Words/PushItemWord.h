#pragma once
#include <string>
#include "../Defines.h"
#include "../StackItems/StackItem.h"
#include "Word.h"

using namespace std;

class Interpreter;

class FORTHICLIB_API PushItemWord : public Word
{
public:
    PushItemWord(string name, shared_ptr<StackItem> item);
    PushItemWord(string name, StackItem* item);
    virtual ~PushItemWord();
    virtual void Execute(Interpreter *interp);

protected:
    shared_ptr<StackItem> item;
};

