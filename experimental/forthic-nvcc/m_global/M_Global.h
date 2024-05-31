#pragma once

#include <memory>
#include <string>
#include <stack>
#include <vector>
#include <map>
#include <chrono>

#include "../Module.h"

using namespace std;
using namespace std::chrono;


class M_Global : public Module
{
public:
    M_Global();

protected:
    virtual shared_ptr<Word> treat_as_literal(string name);

    shared_ptr<Word> treat_as_float(string name);
    shared_ptr<Word> treat_as_int(string name);
};