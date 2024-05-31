#include <string>
#include "ModuleTest.h"
#include "../Module.h"

using namespace std;

ModuleTest::ModuleTest() {
}


void ModuleTest::run() {
    testEmptyModule();
    testAddWord();
    testEnsureVariable();
    testSearchUsingModule();
}


void ModuleTest::testEmptyModule() {
    Module empty_module("");

    // Look for a word
    shared_ptr<Word> w = empty_module.FindWord("POP");
    printFailure(w != nullptr, __FILE__, __LINE__);
}


void ModuleTest::testAddWord() {
    Module module("module");
    module.AddWord(shared_ptr<Word>(new Word("NOP")));

    shared_ptr<Word> w = module.FindWord("NOP");
    printFailure(w == nullptr, __FILE__, __LINE__);
}


void ModuleTest::testEnsureVariable() {
    Module module("module");

    // Check that variable is not present
    shared_ptr<Word> w = module.FindWord("x");
    printFailure(w != nullptr, __FILE__, __LINE__);

    // Add variable and check
    module.EnsureVariable("x");
    w = module.FindWord("x");
    printFailure(w == nullptr, __FILE__, __LINE__);
}


void ModuleTest::testSearchUsingModule() {
    auto used_module =  shared_ptr<Module>(new Module("used-module"));
    used_module->AddWord(shared_ptr<Word>(new Word("HOWDY")));

    Module module("module");

    // HOWDY isn't in module
    shared_ptr<Word> w = module.FindWord("HOWDY");
    printFailure(w != nullptr, __FILE__, __LINE__);

    // HOWDY should be found in the used-module
    module.UseModule(used_module);
    w = module.FindWord("HOWDY");
    printFailure(w == nullptr, __FILE__, __LINE__);
}
