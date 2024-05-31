#pragma once
#include <string>

#include "StackItem.h"
#include "./m_global/I_AsString.h"

using namespace std;


class S_String : public StackItem, public I_AsString
{
public:
    S_String(string s) : item_string(s) {};
    static shared_ptr<S_String> New(string s);
    virtual ~S_String() {};
    string AsString();

    virtual string StringRep();

protected:
    string item_string;
};
