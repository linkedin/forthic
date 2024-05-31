#include <ctime>
#include <cstdio>
#include <cmath>

#include "../Interpreter.h"

#include "../m_global/S_Int.h"
#include "../m_global/S_Address.h"
#include "../m_global/I_AsArray.h"

#include "../m_cuda/M_Cuda.h"
#include "../m_cuda/S_Dim3.h"

#include "M_Gauss.h"


// =============================================================================
// Kernels

#define EPSILON 1E-6

__global__ void pivot(int num_rows, int num_cols, float *A, int pivot_row, int pivot_col) {
    unsigned int idx = threadIdx.x + blockIdx.x * blockDim.x;
    int row = idx / num_cols;
    int col = idx % num_cols;

    // If thread isn't in matrix, return
    if (row >= num_rows || col >= num_cols)   return;

    int pivot_index = pivot_row * num_cols + pivot_col;
    float pivot_coeff = A[pivot_index];

    // If pivot coeff is 0, don't do anything
    if (fabs(pivot_coeff) < EPSILON)   return;

    // Normalize pivot row
    if (row == pivot_row) {
        A[idx] /= pivot_coeff;
    }

    // Synchronize so other threads can pick up the normalized coefficients
    __threadfence();

    float pivot_row_cur_col_coeff = A[pivot_row*num_cols + col];
    float cur_row_pivot_col_coeff = A[row*num_cols + pivot_col];

    // Eliminate pivot
    if (row == pivot_row)                              return;
    else if (fabs(cur_row_pivot_col_coeff) < EPSILON)  return;
    else    A[idx] += -cur_row_pivot_col_coeff * pivot_row_cur_col_coeff;
}


// =============================================================================
// Words


// ( floats num_rows num_cols  -- addr )
class W_GpuMatrix : public Word
{
public:
    W_GpuMatrix(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int num_cols = AsInt(interp->StackPop());
        int num_rows = AsInt(interp->StackPop());
        auto numbers = AsArray(interp->StackPop());

        int num_elements = num_rows * num_cols;
        int num_bytes = num_elements * sizeof(float);

        // Allocate memory
        void* result;
        auto res = cudaMallocManaged((void**)&result, num_bytes);
        checkCudaCall(res, __FILE__, __LINE__);

        // Set values
        float* dst = (float*)result;
        for (int i=0; i < numbers.size(); i++) {
            dst[i] = AsFloat(numbers[i]);
        }

        interp->StackPush(S_Address::New(result));
    }
};

// ( num_rows num_cols  addr -- )
class W_PrintMatrix : public Word
{
public:
    W_PrintMatrix(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        float* A = AsFloatStar(interp->StackPop());
        int num_cols = AsInt(interp->StackPop());
        int num_rows = AsInt(interp->StackPop());

        interp->Run("CUDA-DEVICE-SYNCHRONIZE");

        for (int r=0; r < num_rows; r++) {
            for (int c=0; c < num_cols; c++) {
                int index = c + num_cols*r;
                printf("%6.2f ", A[index]);
            }
            printf("\n");
        }
    }
};


// ( grid block num_rows num_cols addr pivot_row pivot_col -- )
class W_Pivot : public Word
{
public:
    W_Pivot(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int pivot_col = AsInt(interp->StackPop());
        int pivot_row = AsInt(interp->StackPop());
        auto A = AsFloatStar(interp->StackPop());
        int num_cols = AsInt(interp->StackPop());
        int num_rows = AsInt(interp->StackPop());
        dim3 block = AsDim3(interp->StackPop());
        dim3 grid = AsDim3(interp->StackPop());

        pivot<<<grid, block>>>(num_rows, num_cols, A, pivot_row, pivot_col);
    }
};


// =============================================================================
// M_Gauss

M_Gauss::M_Gauss() : Module("gauss") {
    AddWord(new W_GpuMatrix("GPU-MATRIX"));
    AddWord(new W_PrintMatrix("PRINT-MATRIX"));
    AddWord(new W_Pivot("PIVOT"));
}
