#pragma once
#include "../Defines.h"
#include "StackItem.h"

using namespace std;

class FORTHICLIB_API StartArrayItem : public StackItem
{
public:
	StartArrayItem();
	virtual ~StartArrayItem();
};
