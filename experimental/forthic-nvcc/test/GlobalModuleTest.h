#pragma once
#include <string>
#include "Test.h"

class GlobalModuleTest : Test {
public:
    GlobalModuleTest();
    void run();

private:
    void testIntLiteral();
    void testFloatLiteral();
    void testUsingModules();
    void testVariables();
};
