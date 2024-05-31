#include "pch.h"
#include "IntItem.h"


IntItem::IntItem(int _value) : value(_value)
{
}


IntItem::~IntItem()
{
}

int IntItem::GetInt()
{
    return value;
}
