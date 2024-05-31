#include "pch.h"
#include "FloatItem.h"


FloatItem::FloatItem(float _value) : value(_value)
{
}


FloatItem::~FloatItem()
{
}

float FloatItem::GetFloat()
{
    return value;
}
