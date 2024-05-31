#include "pch.h"
#include "CppUnitTest.h"
#include "../ForthicLib/Interpreter.h"
#include "../ForthicLib/StackItems/StackItem.h"
#include "../ForthicLib/StackItems/StringItem.h"
#include "../ForthicLib/StackItems/ArrayItem.h"
#include <string>

using namespace Microsoft::VisualStudio::CppUnitTestFramework;
using namespace std;

namespace ForthicLibTests
{
    TEST_CLASS(InterpreterTest)
    {
    public:
        TEST_METHOD(TestConstruction)
        {
            Interpreter interp;
            Assert::IsNotNull(&interp);
        }

        TEST_METHOD(TestPushString)
        {
            Interpreter interp;
            interp.Run("'Howdy'");
            shared_ptr<StackItem> item = interp.StackPop();
            Assert::AreEqual(string("Howdy"), ForthicGetString(item.get()));
        }

        TEST_METHOD(TestPushEmptyArray)
        {
            Interpreter interp;
            interp.Run("[ ]");
            shared_ptr<StackItem> array_item = interp.StackPop();
            vector<shared_ptr<StackItem>> items = ForthicGetArray(array_item.get());
            Assert::AreEqual(0, (int)items.size());
        }

        TEST_METHOD(TestPushArray)
        {
            Interpreter interp;
            interp.Run("[ 'One' 'Two' ]");
            shared_ptr<StackItem> array_item = interp.StackPop();
            vector<shared_ptr<StackItem>> items = ForthicGetArray(array_item.get());
            Assert::AreEqual(2, (int)items.size());
            Assert::AreEqual(string("One"), ForthicGetString(items[0].get()));
            Assert::AreEqual(string("Two"), ForthicGetString(items[1].get()));
        }

        TEST_METHOD(TestPushModule)
        {
            Interpreter interp;
            interp.Run("{sample");
            auto mod = interp.CurModule();
            Assert::AreEqual(string("sample"), mod->GetName());

            interp.Run("}");
            mod = interp.CurModule();
            Assert::AreEqual(string(""), mod->GetName());
        }

        TEST_METHOD(TestCreateDefinition)
        {
            Interpreter interp;
            interp.Run(": TACO 'taco' ;");
            auto mod = interp.CurModule();
            auto word = mod->FindWord("TACO");
            Assert::AreEqual(string("TACO"), word->GetName());

            // Execute definition
            interp.Run("TACO");
            shared_ptr<StackItem> val = interp.StackPop();
            Assert::AreEqual(string("taco"), ForthicGetString(val.get()));
        }

    };
}