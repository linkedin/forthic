#include "pch.h"
#include "CppUnitTest.h"
#include "../ForthicLib/Words/PushItemWord.h"
#include "../ForthicLib/StackItems/StringItem.h"
#include "../ForthicLib/Modules/Module.h"
#include <string>

using namespace Microsoft::VisualStudio::CppUnitTestFramework;
using namespace std;

namespace ForthicLibTests
{
    TEST_CLASS(ModuleTest)
    {
    public:
        TEST_METHOD(TestConstruction)
        {
            Module empty_module("");
            Assert::IsNotNull(&empty_module);

            // Check that there are no words
            shared_ptr<Word> word = empty_module.FindWord("GREETING");
            Assert::IsTrue(word == nullptr);
        }

        TEST_METHOD(TestAddWord)
        {
            Module module_A("A");
            module_A.AddWord(new PushItemWord("GREETING", new StringItem("Howdy!")));
            shared_ptr<Word> word = module_A.FindWord("GREETING");
            Assert::IsTrue(word != nullptr);
        }

        TEST_METHOD(TestEnsureVariable)
        {
            Module module_A("A");
            module_A.EnsureVariable("x");
            shared_ptr<Word> word = module_A.FindWord("x");
            Assert::IsTrue(word != nullptr);
        }
    };
}
