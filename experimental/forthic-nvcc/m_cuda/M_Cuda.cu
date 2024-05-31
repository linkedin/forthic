#include <cstdio>
#include <sstream>

#include "../Interpreter.h"
#include "../m_global/S_Int.h"
#include "../m_global/S_Address.h"
#include "../S_String.h"

#include "S_Dim3.h"
#include "M_Cuda.h"
#include "S_CudaDeviceProp.h"


// =============================================================================
// Kernels
__global__ void helloFromGPU() {
    printf("Hello from GPU!\n");
}


__global__ void checkIndex() {
    printf("blockIdx:(%d, %d, %d) threadIdx:(%d, %d, %d) blockDim:(%d, %d, %d) gridDim:(%d, %d, %d)\n",
           blockIdx.x, blockIdx.y, blockIdx.z,
           threadIdx.x, threadIdx.y, threadIdx.z,
           blockDim.x, blockDim.y, blockDim.z,
           gridDim.x, gridDim.y, gridDim.z);
}


// =============================================================================
// Words

// ( x y z -- dim3 )
class W_Dim3 : public Word
{
public:
    W_Dim3(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int z = AsInt(interp->StackPop());
        int y = AsInt(interp->StackPop());
        int x = AsInt(interp->StackPop());
        dim3 res(x, y, z);

        interp->StackPush(shared_ptr<S_Dim3>(new S_Dim3(res)));
    }
};


// ( dim3 -- coord )
class W_ToCoord : public Word
{
public:
    W_ToCoord(string name, string coord) : Word(name), coord(coord) {};

    virtual void Execute(Interpreter *interp) {
        dim3 d = AsDim3(interp->StackPop());

        int res = -1;
        if      (coord == "x")   res = d.x;
        else if (coord == "y")   res = d.y;
        else if (coord == "z")   res = d.z;
        else                     throw string("Unknown coord: ") + coord;

        interp->StackPush(shared_ptr<S_Int>(new S_Int(res)));
    }

protected:
    string coord;
};


// ( grid block -- )
class W_CheckIndex : public Word
{
public:
    W_CheckIndex(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        dim3 block = AsDim3(interp->StackPop());
        dim3 grid = AsDim3(interp->StackPop()); 

        checkIndex<<<grid, block>>>();
        cudaDeviceReset();
    }
};


// ( type -- )
class W_Sizeof : public Word
{
public:
    W_Sizeof(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        string type = AsString(interp->StackPop());
        int result = 1;
        if      (type == "FLOAT")    result = sizeof(float);
        else if (type == "INT")      result = sizeof(int);
        interp->StackPush(shared_ptr<S_Int>(new S_Int(result)));
    }
};


// ( address offset num type -- )
class W_PrintMem : public Word
{
public:
    W_PrintMem(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        string type = AsString(interp->StackPop());
        int num = AsInt(interp->StackPop());
        int offset = AsInt(interp->StackPop());
        auto address = interp->StackPop();

        if (type == "FLOAT")    printMemAsFloats(AsFloatStar(address), offset, num);
        else                    printMemAsInts(AsIntStar(address), offset, num);
    }

protected:
    void printMemAsFloats(float* addr, int offset, int num) {
        for (int i=0; i < num ; i++) {
            printf("%-8.4f ", addr[offset+i]);
        }
    }

    void printMemAsInts(int* addr, int offset, int num) {
        for (int i=0; i < num ; i++) {
            printf("%-8d  ", addr[offset+i]);
        }
    }
};


void checkCudaCall(const cudaError_t res, const char* file, int line) {
    if (res != cudaSuccess) {
        stringstream builder;
        builder << cudaGetErrorString(res) << " " << file << ":" << line;
        throw builder.str();
    }
}

// ( index -- )
class W_CudaSetDevice : public Word
{
public:
    W_CudaSetDevice(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int index = AsInt(interp->StackPop());
        auto res = cudaSetDevice(index);
        checkCudaCall(res, __FILE__, __LINE__);
    }
};


// ( -- )
class W_CudaDeviceReset : public Word
{
public:
    W_CudaDeviceReset(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        auto res = cudaDeviceReset();
        checkCudaCall(res, __FILE__, __LINE__);
    }
};


// ( num-bytes -- addr )
class W_CudaMalloc : public Word
{
public:
    W_CudaMalloc(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int num_bytes = AsInt(interp->StackPop());

        void *result;
        auto res = cudaMalloc((void**)&result, num_bytes);
        checkCudaCall(res, __FILE__, __LINE__);
        interp->StackPush(S_Address::New(result));
    }
};


// ( num-bytes -- addr )
class W_CudaMallocManaged : public Word
{
public:
    W_CudaMallocManaged(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int num_bytes = AsInt(interp->StackPop());

        void *result;
        auto res = cudaMallocManaged((void**)&result, num_bytes);
        checkCudaCall(res, __FILE__, __LINE__);
        interp->StackPush(S_Address::New(result));
    }
};


// ( addr -- )
class W_CudaFree : public Word
{
public:
    W_CudaFree(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        void* addr = AsVoidStar(interp->StackPop());
        auto res = cudaFree(addr);
        checkCudaCall(res, __FILE__, __LINE__);
    }
};


// ( -- )
class W_CudaDeviceSynchronize : public Word
{
public:
    W_CudaDeviceSynchronize(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        auto res = cudaDeviceSynchronize();
        checkCudaCall(res, __FILE__, __LINE__);
    }
};


// ( dst src num-bytes -- )
class W_CudaMemcpyHtD : public Word
{
public:
    W_CudaMemcpyHtD(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int num_bytes = AsInt(interp->StackPop());
        void* src = AsFloatStar(interp->StackPop());
        void* dst = AsFloatStar(interp->StackPop());

        auto res = cudaMemcpy(dst, src, num_bytes, cudaMemcpyHostToDevice);
        checkCudaCall(res, __FILE__, __LINE__);
    }
};


// ( dst src num-bytes -- )
class W_CudaMemcpyDtH : public Word
{
public:
    W_CudaMemcpyDtH(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int num_bytes = AsInt(interp->StackPop());
        void* src = AsFloatStar(interp->StackPop());
        void* dst = AsFloatStar(interp->StackPop());

        auto res = cudaMemcpy(dst, src, num_bytes, cudaMemcpyDeviceToHost);
        checkCudaCall(res, __FILE__, __LINE__);
    }
};


// ( devIndex -- cudaDeviceProp )
class W_CudaGetDeviceProperties : public Word
{
public:
    W_CudaGetDeviceProperties(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        int devIndex = AsInt(interp->StackPop());

        cudaDeviceProp deviceProp;
        auto res = cudaGetDeviceProperties(&deviceProp, devIndex);
        checkCudaCall(res, __FILE__, __LINE__);
        interp->StackPush(S_CudaDeviceProp::New(deviceProp));
    }
};


// ( cudaDeviceProp field -- value )
class W_DevProp : public Word
{
public:
    W_DevProp(string name) : Word(name) {};

    virtual void Execute(Interpreter *interp) {
        string field = AsString(interp->StackPop());
        shared_ptr<StackItem> item = interp->StackPop();

        if (auto devPropItem = dynamic_cast<S_CudaDeviceProp*>(item.get())) {
            const cudaDeviceProp& deviceProp = devPropItem->deviceProp();
            if (field == "name") {
                interp->StackPush(S_String::New(string(deviceProp.name)));
            }
            else {
                throw string("Unknown dev prop field: ") + field;
            }
        }
        else {
            throw "Item was not a S_CudaDeviceProp";
        }
    }
};



// =============================================================================
// M_Cuda

M_Cuda::M_Cuda() : Module("cuda")
{
    AddWord(shared_ptr<Word>(new W_Dim3("DIM3")));
    AddWord(shared_ptr<Word>(new W_ToCoord(">x", "x")));
    AddWord(shared_ptr<Word>(new W_ToCoord(">y", "y")));
    AddWord(shared_ptr<Word>(new W_ToCoord(">z", "z")));
    AddWord(shared_ptr<Word>(new W_CheckIndex("GPU-CHECK-INDEX")));
    AddWord(shared_ptr<Word>(new W_Sizeof("SIZEOF")));
    AddWord(shared_ptr<Word>(new W_PrintMem("PRINT-MEM")));
    AddWord(shared_ptr<Word>(new W_CudaSetDevice("CUDA-SET-DEVICE")));
    AddWord(shared_ptr<Word>(new W_CudaDeviceReset("CUDA-DEVICE-RESET")));
    AddWord(shared_ptr<Word>(new W_CudaMalloc("CUDA-MALLOC")));
    AddWord(shared_ptr<Word>(new W_CudaMallocManaged("CUDA-MALLOC-MANAGED")));
    AddWord(shared_ptr<Word>(new W_CudaFree("CUDA-FREE")));
    AddWord(shared_ptr<Word>(new W_CudaDeviceSynchronize("CUDA-DEVICE-SYNCHRONIZE")));
    AddWord(shared_ptr<Word>(new W_CudaMemcpyHtD("CUDA-MEMCPY-HtD")));
    AddWord(shared_ptr<Word>(new W_CudaMemcpyDtH("CUDA-MEMCPY-DtH")));
    AddWord(shared_ptr<Word>(new W_CudaGetDeviceProperties("CUDA-GET-DEVICE-PROPERTIES")));
    AddWord(shared_ptr<Word>(new W_DevProp("DEV-PROP")));
}

string M_Cuda::ForthicCode() {
    string result(
    ": FLOAT   'FLOAT' ; "
    ": INT     'INT' ; "
    );
    return result;
}