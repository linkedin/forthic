#pragma once

#include <cuda_runtime.h>
#include <memory>
#include <string>
#include <stack>
#include <vector>
#include <map>

#include "../Module.h"

using namespace std;


class Ch2Module : public Module
{
public:
    Ch2Module();

protected:
    // virtual shared_ptr<Word> treat_as_literal(string name);
};
