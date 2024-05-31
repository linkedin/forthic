#pragma once
#include <string>

#include "../Defines.h"
#include "../Modules/GlobalModule.h"
#include "StackItem.h"

using namespace std;


class FORTHICLIB_API FloatItem : public StackItem, public IGetFloat
{
public:
	FloatItem(float value);
	virtual ~FloatItem();
	float GetFloat();

protected:
	float value;
};
