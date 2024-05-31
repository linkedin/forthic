 Interpreter.o W_Definition.o W_EndArray.o W_PushItem.o m_global/M_Global.o examples/main.o test/GlobalModuleTest.o test/InterpreterTest.o  : Interpreter.h
  m_cuda/S_Dim3.o : m_cuda/I_AsDim3.h
 examples/main.o m_lp/S_LP.o m_lp/S_LPEquation.o : m_cuda/M_Cuda.h
   : m_cuda/S_CudaDeviceProp.h
   : m_cuda/S_Dim3.h
 examples/main.o  : m_gauss/M_Gauss.h
 m_global/I_AsArray.o m_global/M_Global.o test/InterpreterTest.o m_global/S_Array.o : m_global/I_AsArray.h
 m_global/I_AsFloat.o test/GlobalModuleTest.o m_global/S_Float.o m_global/S_Int.o : m_global/I_AsFloat.h
 m_global/I_AsFloatStar.o m_global/S_Address.o : m_global/I_AsFloatStar.h
 m_global/I_AsInt.o test/GlobalModuleTest.o m_global/S_Float.o m_global/S_Int.o : m_global/I_AsInt.h
 m_global/I_AsIntStar.o m_global/S_Address.o : m_global/I_AsIntStar.h
 m_global/I_AsModule.o m_global/M_Global.o m_global/S_Module.o : m_global/I_AsModule.h
 m_global/I_AsString.o test/InterpreterTest.o S_String.o : m_global/I_AsString.h
 m_global/I_AsTimePoint.o m_global/S_TimePoint.o : m_global/I_AsTimePoint.h
 m_global/I_AsVoidStar.o m_global/S_Address.o : m_global/I_AsVoidStar.h
 m_global/M_Global.o test/GlobalModuleTest.o Interpreter.o : m_global/M_Global.h
 m_global/M_Global.o m_global/S_Address.o  : m_global/S_Address.h
 W_EndArray.o m_global/S_Array.o m_lp/S_LPEquation.o : m_global/S_Array.h
 m_global/M_Global.o m_global/S_Float.o  : m_global/S_Float.h
 m_global/M_Global.o m_global/S_Int.o  : m_global/S_Int.h
 Interpreter.o m_global/S_Module.o  : m_global/S_Module.h
 m_global/M_Global.o m_global/S_TimePoint.o m_global/S_TimePoint.o : m_global/S_TimePoint.h
 examples/main.o  : m_lp/M_LP.h
   : m_lp/S_LPEquation.h
   : m_lp/S_LP.h
 Interpreter.o Module.o m_global/I_AsModule.o m_global/M_Global.o m_global/S_Module.o examples/main.o examples/main.o test/ModuleTest.o Interpreter.o m_cuda/M_Cuda.o m_gauss/M_Gauss.o m_global/M_Global.o m_global/S_Module.o m_global/S_Module.o m_lp/M_LP.o examples/Ch2Module.o : Module.h
 Interpreter.o S_StartArray.o W_EndArray.o  : S_StartArray.h
 Interpreter.o S_String.o m_global/M_Global.o  : S_String.h
 StackItem.o test/InterpreterTest.o Interpreter.o S_StartArray.o S_String.o S_Variable.o W_Definition.o W_EndArray.o W_PushItem.o m_cuda/I_AsDim3.o m_cuda/S_CudaDeviceProp.o m_cuda/S_Dim3.o m_global/I_AsArray.o m_global/I_AsFloat.o m_global/I_AsFloatStar.o m_global/I_AsInt.o m_global/I_AsIntStar.o m_global/I_AsModule.o m_global/I_AsString.o m_global/I_AsTimePoint.o m_global/I_AsVoidStar.o m_global/S_Address.o m_global/S_Array.o m_global/S_Float.o m_global/S_Int.o m_global/S_Module.o m_global/S_TimePoint.o m_lp/S_LP.o m_lp/S_LPEquation.o : StackItem.h
 S_Variable.o Module.o : S_Variable.h
 test/GlobalModuleTest.o test/main_test.o  : test/GlobalModuleTest.h
 test/InterpreterTest.o test/main_test.o  : test/InterpreterTest.h
 test/GlobalModuleTest.o test/ModuleTest.o test/main_test.o test/main_test.o  : test/ModuleTest.h
 test/GlobalModuleTest.o test/InterpreterTest.o test/ModuleTest.o test/Test.o test/TokenizerTest.o test/main_test.o test/main_test.o test/main_test.o test/main_test.o test/GlobalModuleTest.o test/InterpreterTest.o test/ModuleTest.o : test/Test.h
 test/TokenizerTest.o test/main_test.o  : test/TokenizerTest.h
 Token.o Interpreter.o Tokenizer.o : Token.h
 Interpreter.o Tokenizer.o test/TokenizerTest.o  : Tokenizer.h
 W_Definition.o Interpreter.o : W_Definition.h
 Interpreter.o W_EndArray.o  : W_EndArray.h
 Word.o Interpreter.o Module.o W_Definition.o W_EndArray.o W_PushItem.o : Word.h
 Interpreter.o Module.o W_PushItem.o m_global/M_Global.o  : W_PushItem.h
