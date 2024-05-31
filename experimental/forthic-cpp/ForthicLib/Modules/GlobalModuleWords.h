#pragma once

#include <string>
#include <stack>
#include <vector>
#include <map>

#include "../Defines.h"
#include "Words/Word.h"

class Interpreter;

using namespace std;

// ( a -- )
// Pops word from stack
class PopWord : public Word
{
public:
    PopWord(string name);
    virtual void Execute(Interpreter *interp);
};


// ( modules -- )
// Adds modules to current module's using module list
class UseModulesWord : public Word
{
public:
    UseModulesWord(string name);
    virtual void Execute(Interpreter *interp);
};
