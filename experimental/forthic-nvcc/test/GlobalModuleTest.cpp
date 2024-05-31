#include <string>
#include "GlobalModuleTest.h"
#include "../Interpreter.h"
#include "../m_global/M_Global.h"
#include "../m_global/I_AsInt.h"
#include "../m_global/I_AsFloat.h"


using namespace std;

GlobalModuleTest::GlobalModuleTest() {
}

void GlobalModuleTest::run() {
    testIntLiteral();
    testFloatLiteral();
    testUsingModules();
    testVariables();
}


void GlobalModuleTest::testIntLiteral() {
    Interpreter interp;
    interp.Run("27");
    printFailure(27 != AsInt(interp.StackPop()), __FILE__, __LINE__);
}

void GlobalModuleTest::testFloatLiteral() {
    Interpreter interp;
    interp.Run("27.5");
    printFailure(27.5 != AsFloat(interp.StackPop()), __FILE__, __LINE__);
}

void GlobalModuleTest::testUsingModules() {
    Interpreter interp;
    interp.Run("{sample : HI   'Hello' ; } ");

    // Verify that HI is not in the current module's scope
    bool run_failed = false;
    try {
        interp.Run("HI");
    }
    catch(...) {
        run_failed = true;
    }
    printFailure(run_failed == false, __FILE__, __LINE__);

    // If we use USE-MODULES, we can find HI
    interp.Run("[ sample ] USE-MODULES  HI");
}


void GlobalModuleTest::testVariables() {
    Interpreter interp;
    interp.Run("[ 'x' ] VARIABLES");
    interp.Run("21 x !");
    interp.Run("x @");
    printFailure(21 != AsInt(interp.StackPop()), __FILE__, __LINE__);
}
