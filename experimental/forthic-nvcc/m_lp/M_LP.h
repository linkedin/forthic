#pragma once

#include <cuda_runtime.h>
#include <memory>
#include <string>
#include <stack>
#include <vector>
#include <map>

#include "../Module.h"

using namespace std;


class M_LP : public Module
{
public:
    M_LP();

    virtual string ForthicCode();
};
