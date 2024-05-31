#include "pch.h"
#include "StringItem.h"


StringItem::StringItem(string s) : item_string(s)
{
}


StringItem::~StringItem()
{
}

string StringItem::GetString()
{
	return item_string;
}