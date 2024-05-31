#pragma once
#include <string>
#include "StackItem.h"
#include "Word.h"

using namespace std;

class Interpreter;

class W_EndArray : public Word
{
public:
	W_EndArray(string name);
	virtual ~W_EndArray();
	virtual void Execute(Interpreter *interp);
};

