#pragma once
#include <string>

#include "../m_cuda/M_Cuda.h"
#include "../StackItem.h"

using namespace std;


class S_LP : public StackItem
{
public:
    S_LP(Interpreter* interp);

    virtual ~S_LP() {};
    void Free();

    void PrintMatrix();

    virtual string StringRep();
    virtual string AsString();

protected:

    void allocateMatrixMemory();
    void fillMatrixMemory();
    void fillConstraint(int constraintIndex);

protected:
    Interpreter* interp;

    vector<shared_ptr<StackItem>> constraints;
    shared_ptr<StackItem> objective;
    vector<shared_ptr<StackItem>> varnames;

    int num_cols;
    int num_rows;
    int num_elems;
    float *matrix;
};


S_LP* AsLPItem(shared_ptr<StackItem> item);
