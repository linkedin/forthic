#include <algorithm>
#include "W_EndArray.h"
#include "Interpreter.h"
#include "./m_global/S_Array.h"
#include "S_StartArray.h"


W_EndArray::W_EndArray(string word_name) : Word(word_name)
{
}


W_EndArray::~W_EndArray()
{
}

void W_EndArray::Execute(Interpreter *interp)
{
	vector<shared_ptr<StackItem>> result;

	while (true)
	{
		auto item = interp->StackPop();
		if (dynamic_cast<S_StartArray*>(item.get())) break;
		else result.push_back(item);
	}

	std::reverse(result.begin(), result.end());
	interp->StackPush(shared_ptr<StackItem>(new S_Array(result)));
}
