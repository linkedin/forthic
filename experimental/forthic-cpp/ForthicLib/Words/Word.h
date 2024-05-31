#pragma once
#include <string>
#include "../Defines.h"

using namespace std;

class Interpreter;

class FORTHICLIB_API Word
{
public:
	Word(string name);
	virtual ~Word();
	virtual void Execute(Interpreter *interp) = 0;

	string GetName();
protected:
	string name;
};

