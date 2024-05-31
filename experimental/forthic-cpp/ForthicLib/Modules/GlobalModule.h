#pragma once

#include <string>
#include <stack>
#include <vector>
#include <map>

#include "../Defines.h"
#include "../Modules/Module.h"

using namespace std;


class FORTHICLIB_API GlobalModule : public Module
{
public:
    GlobalModule();
    virtual ~GlobalModule();

protected:
    virtual shared_ptr<Word> treat_as_literal(string name);

    shared_ptr<Word> treat_as_float(string name);
    shared_ptr<Word> treat_as_int(string name);
};

class FORTHICLIB_API IGetInt {
public:
    virtual int GetInt() = 0;
};

class FORTHICLIB_API IGetFloat {
public:
    virtual float GetFloat() = 0;
};

int FORTHICLIB_API ForthicGetInt(StackItem *item);
float FORTHICLIB_API ForthicGetFloat(StackItem *item);
