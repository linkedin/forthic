#include <ctime>
#include <cstdio>
#include <cmath>
#include "../Interpreter.h"

#include "../m_global/S_Int.h"
#include "../m_global/S_Array.h"
#include "../m_global/S_Address.h"
#include "../m_global/I_AsString.h"

#include "../m_cuda/M_Cuda.h"
#include "../m_cuda/S_Dim3.h"

#include "M_LP.h"
#include "S_LPEquation.h"
#include "S_LP.h"


// =============================================================================
// Kernels



// =============================================================================
// Words


// ( coeffs name  -- S_LPEquation )
class W_LPEqn : public Word
{
public:
    W_LPEqn(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        string name = AsString(interp->StackPop());
        auto coeffs = AsArray(interp->StackPop());

        interp->StackPush(S_LPEquation::New(coeffs, name));
    }
};



// ( varnames objective constraints  -- LinearProgram )
class W_LPNew : public Word
{
public:
    W_LPNew(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        interp->StackPush(shared_ptr<S_LP>(new S_LP(interp)));
    }

};


// ( LinearProgram -- )
class W_LPPrintMatrix : public Word
{
public:
    W_LPPrintMatrix(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        auto linear_program = AsLPItem(interp->StackPop());
        linear_program->PrintMatrix();
    }

};


// ( LinearProgram -- )
class W_LPFree : public Word
{
public:
    W_LPFree(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        auto linear_program = AsLPItem(interp->StackPop());
        linear_program->Free();
    }
};


// =============================================================================
// M_LP

M_LP::M_LP() : Module("linear-program") {
    AddWord(new W_LPNew("LP-NEW"));
    AddWord(new W_LPFree("LP-FREE"));
    AddWord(new W_LPPrintMatrix("LP-PRINT-MATRIX"));
    AddWord(new W_LPEqn("LP-EQN"));
}

string M_LP::ForthicCode() {
    string result("[ gauss ] USE-MODULES");
    return result;
}
