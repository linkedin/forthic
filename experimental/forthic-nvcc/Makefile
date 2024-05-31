LIB_OBJECTS       = Token.o Tokenizer.o Module.o Word.o StackItem.o \
                    S_Variable.o W_PushItem.o \
                    S_String.o S_StartArray.o W_EndArray.o \
                    W_Definition.o Interpreter.o \
                    m_global/I_AsFloatStar.o m_global/I_AsIntStar.o m_global/I_AsVoidStar.o \
                    m_global/M_Global.o m_global/S_Int.o m_global/S_Float.o \
                    m_global/I_AsFloat.o m_global/I_AsInt.o m_global/I_AsTimePoint.o \
                    m_global/S_Address.o m_global/S_TimePoint.o \
                    m_global/S_Array.o m_global/S_Module.o \
                    m_global/I_AsArray.o m_global/I_AsModule.o m_global/I_AsString.o \
                    m_cuda/M_Cuda.o m_cuda/S_Dim3.o m_cuda/I_AsDim3.o \
                    m_cuda/S_CudaDeviceProp.o m_gauss/M_Gauss.o m_lp/M_LP.o \
                    m_lp/S_LPEquation.o m_lp/S_LP.o \
                    examples/Ch2Module.o
APP_OBJECTS       = examples/main.o $(LIB_OBJECTS)
TEST_OBJECTS      = test/Test.o test/TokenizerTest.o test/ModuleTest.o \
                    test/InterpreterTest.o test/GlobalModuleTest.o
TEST_APP_OBJECTS  = test/main_test.o $(TEST_OBJECTS) $(LIB_OBJECTS)

all: examples/app test runtest

examples/app: $(APP_OBJECTS)
	nvcc -o examples/app $(APP_OBJECTS) -lncurses

.PHONY: runtest
runtest:
	./test/test

.PHONY: runapp
runapp: examples/app
	cd examples && ./app BHM-p.62-LP.forthic

test: $(TEST_APP_OBJECTS)
	nvcc -o ./test/test $(TEST_APP_OBJECTS) -lncurses

.PHONY: clean
clean:
	rm -f $(APP_OBJECTS) app
	rm -f $(TEST_APP_OBJECTS) ./test/test

%.o:%.cpp %.h
	nvcc -std=c++11 -g -c -o $@ $<

%.o:%.cu %.h
	nvcc -arch=sm_30 -std=c++11 -g -c -o $@ $<

examples/main.o:examples/main.cpp
	nvcc -std=c++11 -g -c -o $@ $<

.PHONY: deps
deps:
	python3 deps.py > deps.mk 2>/dev/null

# Dependencies (generate with python3 dep.py)
include deps.mk
