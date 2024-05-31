#pragma once
#include <string>
#include <vector>

#include "../Defines.h"
#include "../StackItems/StackItem.h"

class Module;

using namespace std;

class FORTHICLIB_API IGetString {
public:
	virtual string GetString() = 0;
};

class FORTHICLIB_API IGetArray {
public:
	virtual vector<shared_ptr<StackItem>> GetArray() = 0;
};

class FORTHICLIB_API IGetValue {
public:
    virtual shared_ptr<StackItem> GetValue() = 0;
};

class FORTHICLIB_API IGetModule {
public:
    virtual shared_ptr<Module> GetModule() = 0;
};

string FORTHICLIB_API ForthicGetString(StackItem *item);
vector<shared_ptr<StackItem>> FORTHICLIB_API ForthicGetArray(StackItem *item);
shared_ptr<StackItem> FORTHICLIB_API ForthicGetValue(StackItem *item);
shared_ptr<Module> FORTHICLIB_API ForthicGetModule(StackItem *item);
