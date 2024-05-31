#include "pch.h"
#include "VariableItem.h"

VariableItem::VariableItem() : value(nullptr)
{
}

VariableItem::~VariableItem()
{
}

shared_ptr<StackItem> VariableItem::GetValue()
{
    return value;
}

void VariableItem::SetValue(shared_ptr<StackItem> new_value)
{
    value = new_value;
}
