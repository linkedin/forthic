#pragma once
#include <string>

using namespace std;

class Interpreter;

class Word
{
public:
    Word(string name);
    virtual ~Word();
    virtual void Execute(Interpreter *interp);

    string GetName();
protected:
    string name;
};

