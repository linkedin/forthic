#pragma once
#include <string>
#include "../Defines.h"
#include "../StackItems/StackItem.h"
#include "Word.h"

using namespace std;

class Interpreter;

class FORTHICLIB_API EndArrayWord : public Word
{
public:
	EndArrayWord(string name);
	virtual ~EndArrayWord();
	virtual void Execute(Interpreter *interp);
};

