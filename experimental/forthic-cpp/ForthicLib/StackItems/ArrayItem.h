#pragma once
#include <string>
#include <vector>

#include "../Defines.h"
#include "../Modules/GlobalItemGetters.h"
#include "StackItem.h"

using namespace std;


class FORTHICLIB_API ArrayItem : public StackItem, public IGetArray
{
public:
	ArrayItem(vector<shared_ptr<StackItem>> items);
	virtual ~ArrayItem();
	vector<shared_ptr<StackItem>> GetArray();

protected:
	vector<shared_ptr<StackItem>> items;
};