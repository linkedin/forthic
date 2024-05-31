#include <cstdio>
#include <string>
#include "TokenizerTest.h"
#include "ModuleTest.h"
#include "InterpreterTest.h"
#include "GlobalModuleTest.h"

using namespace std;

int main() {
    TokenizerTest tokenizerTest;
    ModuleTest moduleTest;
    InterpreterTest interpTest;
    GlobalModuleTest globalModuleTest;

    try {
        tokenizerTest.run();
        moduleTest.run();
        interpTest.run();
        globalModuleTest.run();
    }
    catch (const char *message) {
        printf("EXCEPTION: %s\n", message);
    }
    catch (string message) {
        printf("EXCEPTION: %s\n", message.c_str());
    }
    return 0;
}
