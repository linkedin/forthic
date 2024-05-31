#pragma once
#include <string>
#include "Test.h"

class ModuleTest : Test {
public:
    ModuleTest();
    void run();

private:
    void testEmptyModule();
    void testAddWord();
    void testEnsureVariable();
    void testSearchUsingModule();
};
