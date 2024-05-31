#include "pch.h"
#include "ArrayItem.h"


ArrayItem::ArrayItem(vector<shared_ptr<StackItem>>_items) : items(_items)
{
}


ArrayItem::~ArrayItem()
{
}

vector<shared_ptr<StackItem>> ArrayItem::GetArray()
{
	return items;
}