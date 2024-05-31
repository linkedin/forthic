#include "pch.h"
#include <algorithm>
#include "EndArrayWord.h"
#include "../Interpreter.h"
#include "../StackItems/ArrayItem.h"
#include "../StackItems/StartArrayItem.h"


EndArrayWord::EndArrayWord(string word_name) : Word(word_name)
{
}


EndArrayWord::~EndArrayWord()
{
}

void EndArrayWord::Execute(Interpreter *interp)
{
	vector<shared_ptr<StackItem>> result;

	while (true)
	{
		auto item = interp->StackPop();
		if (dynamic_cast<StartArrayItem*>(item.get())) break;
		else result.push_back(item);
	}

	std::reverse(result.begin(), result.end());
	interp->StackPush(shared_ptr<StackItem>(new ArrayItem(result)));
}
