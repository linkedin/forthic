#pragma once
#include "Test.h"

class InterpreterTest : Test {
public:
    InterpreterTest();
    void run();

private:
    void testPushString();
    void testPushEmptyArray();
    void testPushArray();
    void testPushModule();
    void testCreateDefinition();
};