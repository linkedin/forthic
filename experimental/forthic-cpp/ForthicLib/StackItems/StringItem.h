#pragma once
#include <string>

#include "../Defines.h"
#include "../Modules/GlobalItemGetters.h"
#include "StackItem.h"

using namespace std;


class FORTHICLIB_API StringItem : public StackItem, public IGetString
{
public:
	StringItem(string s);
	virtual ~StringItem();
	string GetString();

protected:
	string item_string;
};