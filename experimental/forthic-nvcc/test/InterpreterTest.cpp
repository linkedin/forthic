#include <string>
#include "InterpreterTest.h"
#include "../Interpreter.h"
#include "../StackItem.h"
#include "../m_global/I_AsString.h"
#include "../m_global/I_AsArray.h"

using namespace std;

InterpreterTest::InterpreterTest() {
}

void InterpreterTest::run() {
    testPushString();
    testPushEmptyArray();
    testPushArray();
    testPushModule();
    testCreateDefinition();
}

void InterpreterTest::testPushString() {
    Interpreter interp;
    interp.Run("'Howdy'");
    printFailure(string("Howdy") != AsString(interp.StackPop()), __FILE__, __LINE__);
}

void InterpreterTest::testPushEmptyArray() {
    Interpreter interp;
    interp.Run("[ ]");
    auto items = AsArray(interp.StackPop());
    printFailure(items.size() != 0, __FILE__, __LINE__);
}

void InterpreterTest::testPushArray() {
    Interpreter interp;
    interp.Run("[ 'One' 'Two' ]");
    auto items = AsArray(interp.StackPop());
    printFailure(2 != (int)items.size(), __FILE__, __LINE__);
    printFailure(string("One") != AsString(items[0]), __FILE__, __LINE__);
    printFailure(string("Two") != AsString(items[1]), __FILE__, __LINE__);
}


void InterpreterTest::testPushModule() {
    Interpreter interp;
    interp.Run("{sample");
    auto mod = interp.CurModule();
    printFailure(string("sample") != mod->GetName(), __FILE__, __LINE__);

    interp.Run("}");
    mod = interp.CurModule();
    printFailure(string("") != mod->GetName(), __FILE__, __LINE__);
}


void InterpreterTest::testCreateDefinition() {
    Interpreter interp;
    interp.Run(": TACO 'taco' ;");
    auto mod = interp.CurModule();
    auto word = mod->FindWord("TACO");
    printFailure(string("TACO") != word->GetName(), __FILE__, __LINE__);

    // Execute definition
    interp.Run("TACO");
    printFailure(string("taco") != AsString(interp.StackPop()), __FILE__, __LINE__);
}
