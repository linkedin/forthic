#pragma once
#include <string>

#include "../Defines.h"
#include "../Modules/GlobalModule.h"
#include "StackItem.h"

using namespace std;


class FORTHICLIB_API IntItem : public StackItem, public IGetInt
{
public:
	IntItem(int value);
	virtual ~IntItem();
	int GetInt();

protected:
	int value;
};
