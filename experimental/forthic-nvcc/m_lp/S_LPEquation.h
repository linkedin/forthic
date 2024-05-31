#pragma once
#include <string>

#include "../StackItem.h"

#include "../m_global/S_Array.h"

#include "../m_cuda/M_Cuda.h"

using namespace std;


class S_LPEquation : public StackItem
{
public:
    S_LPEquation(vector<shared_ptr<StackItem>> coeffs, string name);
    virtual ~S_LPEquation();

    static shared_ptr<S_LPEquation> New(vector<shared_ptr<StackItem>> coeffs, string name);

    string GetName() { return name; }

    virtual string StringRep();
    virtual string AsString();

    int NumCoeffs() { return num_coeffs; }
    const float* Coeffs() { return coeffs; }

protected:
    string name;
    int num_coeffs;
    float* coeffs;
};


S_LPEquation* AsLPEquationItem(shared_ptr<StackItem> item);
