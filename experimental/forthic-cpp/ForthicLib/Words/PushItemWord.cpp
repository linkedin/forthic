#include "pch.h"
#include "PushItemWord.h"
#include "../Interpreter.h"


PushItemWord::PushItemWord(string word_name, shared_ptr<StackItem> i) : Word(word_name), item(i)
{
}

PushItemWord::PushItemWord(string name, StackItem* item) : PushItemWord(name, shared_ptr<StackItem>(item))
{
}

PushItemWord::~PushItemWord()
{
	// The item shouldn't be deleted here. Whover pops it from the stack should delete it.
}

void PushItemWord::Execute(Interpreter *interp)
{
	interp->StackPush(item);
}
