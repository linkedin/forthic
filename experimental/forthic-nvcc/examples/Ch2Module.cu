#include <ctime>
#include <cstdio>
#include "../Interpreter.h"
#include "../m_global/S_Int.h"
#include "../m_global/I_AsFloatStar.h"
#include "../m_global/I_AsIntStar.h"
#include "../m_global/I_AsVoidStar.h"

#include "../m_cuda/M_Cuda.h"
#include "../m_cuda/S_Dim3.h"

#include "Ch2Module.h"


// =============================================================================
// Kernels

__global__ void sumArraysOnGPU(float *A, float *B, float *C, const int N) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;

    if (i < N)   C[i] = A[i] + B[i];
}

__global__ void printThreadIndex(int *A, const int nx, const int ny)
{
    int ix = threadIdx.x + blockIdx.x * blockDim.x;
    int iy = threadIdx.y + blockIdx.y * blockDim.y;
    unsigned int idx = iy * nx + ix;

    printf("thread_id (%d,%d) block_id (%d,%d) coordinate (%d,%d) global index"
           " %2d ival %2d\n", threadIdx.x, threadIdx.y, blockIdx.x, blockIdx.y,
           ix, iy, idx, A[idx]);
}

__global__ void sumMatrixOnGPU2DBlock2DGrid(float *MatA, float *MatB, float *MatC, int nx, int ny)
{
    unsigned int ix = threadIdx.x + blockIdx.x * blockDim.x;
    unsigned int iy = threadIdx.y + blockIdx.y * blockDim.y;
    unsigned int idx = iy * nx + ix;

    if (ix < nx && iy < ny)
        MatC[idx] = MatA[idx] + MatB[idx];
}

// grid 2D block 1D
__global__ void sumMatrixOnGPU1DBlock2DGrid(float *MatA, float *MatB, float *MatC, int nx, int ny)
{
    unsigned int ix = threadIdx.x + blockIdx.x * blockDim.x;
    unsigned int iy = blockIdx.y;
    unsigned int idx = iy * nx + ix;

    if (ix < nx && iy < ny)
        MatC[idx] = MatA[idx] + MatB[idx];
}

// =============================================================================
// Words


// ( hostref gpuref num -- int )
class CheckResultWord : public Word
{
public:
    CheckResultWord(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int num = AsInt(interp->StackPop());
        float* gpuRef = AsFloatStar(interp->StackPop());
        float* hostRef = AsFloatStar(interp->StackPop());

        double epsilon = 1.0E-8;
        bool match = 1;

        for (int i = 0; i < num; i++) {
            if (abs(hostRef[i] - gpuRef[i]) > epsilon) {
                match = 0;
                printf("Arrays do not match!\n");
                printf("host %5.2f gpu %5.2f at current %d\n", hostRef[i],
                       gpuRef[i], i);
                break;
            }
        }
        interp->StackPush(shared_ptr<S_Int>(new S_Int(match)));
    }
};


// ( addr num -- )
class InitDataWord : public Word
{
public:
    InitDataWord(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int num = AsInt(interp->StackPop());
        float* addr = AsFloatStar(interp->StackPop());

        for (int i = 0; i < num; i++) {
            addr[i] = (float)(rand() & 0xFF) / 10.0f;
        }
    }
};

// ( addr-A addr-B addr-C n -- )
class HSumArraysWord : public Word
{
public:
    HSumArraysWord(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int N = AsInt(interp->StackPop());
        auto C = AsFloatStar(interp->StackPop());
        auto B = AsFloatStar(interp->StackPop());
        auto A = AsFloatStar(interp->StackPop());

        for (int idx = 0; idx < N; idx++)    C[idx] = A[idx] + B[idx];
    }
};

// ( grid block addr-A addr-B addr-C n -- )
class DSumArraysWord : public Word
{
public:
    DSumArraysWord(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int N = AsInt(interp->StackPop());
        auto d_C = AsFloatStar(interp->StackPop());
        auto d_B = AsFloatStar(interp->StackPop());
        auto d_A = AsFloatStar(interp->StackPop());
        int block = AsInt(interp->StackPop());
        int grid = AsInt(interp->StackPop());

        sumArraysOnGPU<<<grid, block>>>(d_A, d_B, d_C, N);
    }
};


// ( addr size -- )
class InitialIntWord : public Word
{
public:
    InitialIntWord(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int size = AsInt(interp->StackPop());
        int* A = AsIntStar(interp->StackPop());
        for (int i=0; i < size; i++) {
            A[i] = i;
        }
    }
};


// ( addr-C nx ny -- )
/*
 * This example helps to visualize the relationship between thread/block IDs and
 * offsets into data. For each CUDA thread, this example displays the
 * intra-block thread ID, the inter-block block ID, the global coordinate of a
 * thread, the calculated offset into input data, and the input data at that
 * offset.
 */
class PrintIntMatrixWord : public Word
{
public:
    PrintIntMatrixWord(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int ny = AsInt(interp->StackPop());
        int nx = AsInt(interp->StackPop());
        int* C = AsIntStar(interp->StackPop());

        int *ic = C;
        printf("\nMatrix: (%d.%d)\n", nx, ny);

        for (int iy = 0; iy < ny; iy++) {
            for (int ix = 0; ix < nx; ix++) {
                printf("%3d", ic[ix]);
            }

            ic += nx;  // Advance to next row
            printf("\n");
        }

        printf("\n");
        return;
    }
};


// ( grid block addr-A nx ny -- )
class PrintThreadIndexWord : public Word
{
public:
    PrintThreadIndexWord(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int ny = AsInt(interp->StackPop());
        int nx = AsInt(interp->StackPop());
        int* A = AsIntStar(interp->StackPop());
        dim3 block = AsDim3(interp->StackPop());
        dim3 grid = AsDim3(interp->StackPop());

        printThreadIndex<<<grid, block>>>(A, nx, ny);
    }
};


// ( addr-A addr-B addr-C nx ny -- )
class HSumMatricesWord : public Word
{
public:
    HSumMatricesWord(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int ny = AsInt(interp->StackPop());
        int nx = AsInt(interp->StackPop());
        float* C = AsFloatStar(interp->StackPop());
        float* B = AsFloatStar(interp->StackPop());
        float* A = AsFloatStar(interp->StackPop());

        float *ia = A;
        float *ib = B;
        float *ic = C;

        for (int iy = 0; iy < ny; iy++) {
            for (int ix = 0; ix < nx; ix++) {
                ic[ix] = ia[ix] + ib[ix];
            }
            ia += nx; ib += nx; ic += nx;
        }
    }
};


// ( grid block  addr-A addr-B addr-C nx ny -- )
class DSumMatricesWord : public Word
{
public:
    DSumMatricesWord(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int ny = AsInt(interp->StackPop());
        int nx = AsInt(interp->StackPop());
        float* C = AsFloatStar(interp->StackPop());
        float* B = AsFloatStar(interp->StackPop());
        float* A = AsFloatStar(interp->StackPop());
        dim3 block = AsDim3(interp->StackPop());
        dim3 grid = AsDim3(interp->StackPop());

        sumMatrixOnGPU2DBlock2DGrid<<<grid, block>>>(A, B, C, nx, ny);
    }
};


// ( grid block  addr-A addr-B addr-C nx ny -- )
class DSumMatrices2DGrid1DBlockWord : public Word
{
public:
    DSumMatrices2DGrid1DBlockWord(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int ny = AsInt(interp->StackPop());
        int nx = AsInt(interp->StackPop());
        float* C = AsFloatStar(interp->StackPop());
        float* B = AsFloatStar(interp->StackPop());
        float* A = AsFloatStar(interp->StackPop());
        dim3 block = AsDim3(interp->StackPop());
        dim3 grid = AsDim3(interp->StackPop());

        sumMatrixOnGPU1DBlock2DGrid<<<grid, block>>>(A, B, C, nx, ny);
    }
};



// =============================================================================
// Ch2Module

Ch2Module::Ch2Module() : Module("ch2") {
    AddWord(shared_ptr<Word>(new CheckResultWord("CHECK-RESULT")));
    AddWord(shared_ptr<Word>(new InitDataWord("INIT-DATA")));
    AddWord(shared_ptr<Word>(new HSumArraysWord("H-SUM-ARRAYS")));
    AddWord(shared_ptr<Word>(new DSumArraysWord("D-SUM-ARRAYS")));

    AddWord(shared_ptr<Word>(new InitialIntWord("INITIAL-INT")));
    AddWord(shared_ptr<Word>(new PrintIntMatrixWord("PRINT-INT-MATRIX")));
    AddWord(shared_ptr<Word>(new PrintThreadIndexWord("PRINT-THREAD-INDEX")));

    AddWord(shared_ptr<Word>(new HSumMatricesWord("H-SUM-MATRICES")));
    AddWord(shared_ptr<Word>(new DSumMatricesWord("D-SUM-MATRICES")));
    AddWord(shared_ptr<Word>(new DSumMatrices2DGrid1DBlockWord("D-SUM-MATRICES-2DGRID-1DBLOCK")));
}
