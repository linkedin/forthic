#include <sstream>
#include "../Interpreter.h"

#include "../m_global/S_Address.h"
#include "../m_global/S_Float.h"
#include "../m_global/S_Int.h"

#include "S_LP.h"
#include "S_LPEquation.h"


// ( varnames objective constraints  )
S_LP::S_LP(Interpreter* interp) : interp(interp) {
    constraints = AsArray(interp->StackPop());
    objective = interp->StackPop();
    varnames = AsArray(interp->StackPop());

    // structural + logical + RHS
    num_cols = varnames.size() + constraints.size() + 1;

    // objective + constraints
    num_rows = 1 + constraints.size();

    num_elems = num_rows * num_cols;

    // Allocate memory
    allocateMatrixMemory();
    // TODO: Allocate ratio memory

    fillMatrixMemory();
}


void S_LP::Free() {
    interp->StackPush(S_Address::New((void*)matrix));
    interp->Run("CUDA-FREE");

    // TODO: Free ratio memory
}


void S_LP::PrintMatrix() {
    interp->StackPush(shared_ptr<S_Int>(new S_Int(num_rows)));
    interp->StackPush(shared_ptr<S_Int>(new S_Int(num_cols)));
    interp->StackPush(S_Address::New((void*) matrix));
    interp->Run("PRINT-MATRIX");
}


void S_LP::allocateMatrixMemory() {
    int num_bytes = num_elems * sizeof(float);
    interp->StackPush(shared_ptr<S_Int>(new S_Int(num_bytes)));
    interp->Run("CUDA-MALLOC-MANAGED");
    matrix = AsFloatStar(interp->StackPop());
    for (int i=0; i < num_elems; i++)   matrix[i] = 0.0;
}


void S_LP::fillMatrixMemory() {
    int col = 0;

    // Objective
    auto obj_eq = AsLPEquationItem(objective);
    int num_coeffs = obj_eq->NumCoeffs();
    const float* coeffs = obj_eq->Coeffs();
    for (int i=0; i < num_coeffs; i++)   matrix[col++] = coeffs[i];

    // Constraints
    for (int i=0; i < constraints.size(); i++)   fillConstraint(i);
}


void S_LP::fillConstraint(int constraintIndex) {
    int col=0;
    int offset = (constraintIndex + 1) * num_cols;  // Objective is in the first row
    auto constraint_eq = AsLPEquationItem(constraints[constraintIndex]);
    int num_coeffs = constraint_eq->NumCoeffs();
    const float* coeffs = constraint_eq->Coeffs();

    // Add structural coeffs
    for (int i=0; i < num_coeffs; i++)    matrix[offset+col++] = coeffs[i];

    // Add logical coeff
    matrix[offset+col+constraintIndex] = 1.0;
}


string S_LP::StringRep() {
    stringstream builder;
    builder << "S_LP";
    return builder.str();
}

string S_LP::AsString() {
    return StringRep();
}



S_LP* AsLPItem(shared_ptr<StackItem> item) {
    if (auto i = dynamic_cast<S_LP*>(item.get())) {
        return i;
    }
    else {
        throw item->StringRep() + " is not an S_LP";
    }
}
